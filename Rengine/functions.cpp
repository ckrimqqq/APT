#include <Rcpp.h>
#include <queue>

using namespace Rcpp;

/*
 * Functions that were written in C++ for performance. 
 * 
 * Functions here are not supposed to be used directly within rmd. Most of them
 * are executed by wrappers in functions.R and mdutils.R files.
 */

typedef std::vector<std::string> str_vec;
typedef std::vector<int> int_vec;
typedef std::map<std::string, int> count_map;
typedef std::map<std::string, std::string> word_map;

/**
 * Recalculates total number of running users from the vector of users local to 
 * each load generator.
 *  
 * @param users integer vector of running users (local to load generator).
 * @param machines character vector of the same length as users with machine
 * label per each measurement to distinguish between load generators.
 * @return new integer vector with the same length as users with best guess of 
 * real number of running users accross multiple load generators.
 */
// [[Rcpp::export(name = ".fix_users_cpp")]]
int_vec fix_distributed_users(int_vec users, str_vec machines) {
    int_vec users_fixed(users.size(), 0);
    count_map mstate;
    int_vec::iterator usr;
    int_vec::iterator nusr;
    str_vec::iterator mch;
    count_map::iterator sentry;
    int prev = 0, usr_prev = 0;
    
    for (usr = users.begin(), mch = machines.begin(), nusr = users_fixed.begin();
         usr != users.end() && mch != machines.end(); 
         ++usr, ++mch, ++nusr) {
        sentry = mstate.find(*mch);
        usr_prev = (sentry == mstate.end() ? 0 : sentry->second);
        
        prev += (*usr - usr_prev);
        mstate[*mch] = *usr;
        *nusr = prev;
    }
    return users_fixed;
}

/**
 * Calculates moving sample mean for a real valued (numeric) sample. In this 
 * context movmean means average value of metric for past n successfull samples.
 * 
 * @param metrics numeric vector of measurements.
 * @param isValid logical vector with the same length as metrics. Represents 
 * validity status of the test (m.avg. is calculated using only successfull
 * samples).
 * @param n number of samples for mean calculation.
 * @return new numeric vector with the same length as metrics containing moving 
 * mean for each measurement.
 */
// [[Rcpp::export(name = ".mov_mean")]]
std::vector<double> mov_mean(std::vector<double> metrics,
                             std::vector<bool> isValid,
                             unsigned int n) {
    std::vector<double> mmean_v(metrics.size(), 0);
    std::queue<double> backlog;
    std::vector<double>::iterator src;
    std::vector<bool>::iterator flg;
    std::vector<double>::iterator dst;
    double curr = NA_REAL;
    
    for (src = metrics.begin(), flg = isValid.begin(), dst = mmean_v.begin();
         src != metrics.end() && flg != isValid.end(); 
         ++src, ++flg, ++dst) {
        if (backlog.size() > 0) {
            *dst = curr / backlog.size();
        } else {
            *dst = NA_REAL;
        }
        if (*flg && !NumericVector::is_na(*src)) {
            if (backlog.size() == n) {
                double fst = backlog.front();
                backlog.pop();
                curr -= fst;
            }
            backlog.push(*src);
            
            if (NumericVector::is_na(curr)) 
                curr = *src;
            else
                curr += *src;
        }
    }
    return mmean_v;
}


/**
 * Helper function that converts 2 character arrays of keys/values to single
 * std::map.
 * 
 * @param wm reference to target map.
 * @param keys vector of strings with map keys.
 * @param values vector of strings with map values (the same length as keys).
 */
void fill_lookup_map(word_map& wm, str_vec& keys, str_vec& values) {
    str_vec::iterator key, val;
    
    for (key = keys.begin(), val = values.begin();
         key != keys.end() && val != values.end();
         ++key, ++val)
        wm[*key] = *val;
}

/**
 * Assing business groups to transactions according to user-mapping.
 * 
 * @param names transaction names vector.
 * @param prefixes character vector with transaction prefixes.
 * @param groups character vector of the same length as prefixes that maps 
 * business group to corresponding transaction prefix.
 * @param default_group group by default (if transaction doesn't match any 
 * prefix).
 * @return new character vector with the same length as names containing 
 * business group for each transaction.
 */
// [[Rcpp::export(name = ".assign_bgroups")]]
str_vec assign_bgroups(str_vec names, 
                       str_vec prefixes, 
                       str_vec groups,
                       std::string default_group) {
    str_vec result(names.size(), default_group);
    str_vec::iterator nm, res;
    word_map lookup;
    word_map::iterator lookup_it;
    std::string key, gr;
    std::pair<std::string::iterator, std::string::iterator> mm_pair;
    
    fill_lookup_map(lookup, prefixes, groups);
    for (nm = names.begin(), res = result.begin(); 
         nm != names.end(); 
         ++nm, ++res) {
        lookup_it = lookup.upper_bound(*nm);
        if (lookup_it != lookup.begin()) {
            key = (--lookup_it)->first;
            gr = lookup_it->second;
            if (key.size() <= nm->size()) {
                mm_pair = std::mismatch(key.begin(), key.end(), nm->begin());
                if (mm_pair.first == key.end())
                    *res = gr;
            }
        }
    }
    return result;
}

/**
 * Fast rename function for logical vectors (no char conversion is required).
 * 
 * @param names logical vector.
 * @param name_pos label for successful transactions.
 * @param name_neg label for failed transactions.
 * @return new character vector with the same length as names containing 
 * correct labels.
 */
// [[Rcpp::export(name = ".rename_logical")]]
str_vec rename_logical(std::vector<bool> names, 
                       std::string name_pos, 
                       std::string name_neg) {
    str_vec result(names.size(), name_neg);
    std::vector<bool>::iterator name;
    str_vec::iterator res;
    
    for (name = names.begin(), res = result.begin(); 
         name != names.end(); ++name, ++res) {
        if (*name)
            *res = name_pos;
    }
    return result;
}

/**
 * Calculate number of items laying within specified interval length. 
 * 
 * @param metrics vector of metrics.
 * @param intervals sorted interval boundaries.
 * @return new integer vector of item counts per interval.
 */
// [[Rcpp::export(name = ".grouped_count")]]
int_vec grouped_count(std::vector<double> metrics,
                      std::vector<double> intervals) {
    std::sort(metrics.begin(), metrics.end());
    int_vec result;
    std::vector<double>::iterator it;
    std::vector<double>::iterator bound = intervals.begin();
    int count = 0;
    for (it = metrics.begin(), ++bound; it != metrics.end(); ++it) {
        while (*it >= *bound && bound != intervals.end()) {
            result.push_back(count);
            count = 0;
            ++bound;
        }
        ++count;
    }
    while (bound != intervals.end()) {
        result.push_back(count);
        count = 0;
        ++bound;
    }
    result.push_back(count);
    return result;
}
