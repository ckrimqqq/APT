(function() {
	function sort(table, oTH, drop) {
		var trs = [].slice.call(table.tBodies[0].getElementsByTagName('tr'), (table.tHead == null ? 1 : 0));
		var sortCol = [].slice.call(oTH.parentNode.querySelectorAll('th[lvl]'));
		sortCol.sort(function(a, b){
				return a.getAttribute('lvl') - b.getAttribute('lvl');
			});
		var alreadySelectedCol = sortCol.indexOf(oTH) >= 0;
		var sortColLength = sortCol.length
		if (drop && !((sortColLength == 1) && alreadySelectedCol)) {
			for (var el = 0, len = sortColLength; el < len; el++) {
				sortCol[el].className = '';
				sortCol[el].removeAttribute('lvl');
			}
			sortCol = [oTH];
			oTH.setAttribute('lvl', 1);
		} else if (!alreadySelectedCol) {
			sortCol.push(oTH);
			oTH.setAttribute('lvl', sortCol.length);
		}
		oTH.className = oTH.className == 'ascend' ? 'descend' : 'ascend';
		if ((sortColLength == 1) && alreadySelectedCol) {
			trs.reverse();
		} else {
			trs.sort(
				function(a, b){
					var row1 = a.cells,
						row2 = b.cells;
						
					for (var el = 0, len = sortCol.length; el < len; el++) {
						var col = sortCol[el].cellIndex,
							v1 = row1[col].innerText,
							v2 = row2[col].innerText;
						if (((!isNaN(parseFloat(v1)) && isFinite(v1)) || v1 === "NA") && ((!isNaN(parseFloat(v2)) && isFinite(v2)) || v2 === "NA")) {
							v1 = v1 === "NA" ? Number.NEGATIVE_INFINITY : +v1;
							v2 = v2 === "NA" ? Number.NEGATIVE_INFINITY : +v2;
						}
						if (v1 == v2) {
							continue;
						} else {
							var cmp = v1 > v2 ? 1 : -1;
							return sortCol[el].className == 'ascend' ? cmp : -cmp;
						}
					}
					return 0;
				});
		}
		for (i = 0; i < trs.length; ++i) {
			table.tBodies[0].appendChild(trs[i]);
		}
	}

	var CollapsibleLists = function () {
		function toggle(node, force) {
			var open = (force === void(0) ? !node.classList.contains("Closed") : !!force)
			if (node.classList.contains("collapsibleList")) {
				if (open) {
					node.classList.add("Closed");
				} else {
					node.classList.remove("Closed");
				}
			}
		}

		this.expand = function (openAll) {
			var lis = this.root.getElementsByTagName('li');
			for (var index = 0; index < lis.length; index ++) {
				toggle(lis[index], openAll);
			}
		};

		this.init = function (root) {
			this.root = root;
			var lis = root.getElementsByTagName('li');
			for (var index = 0; index < lis.length; index ++) {
				var li = lis[index];
				if (li.querySelector('ul') !== null) {
					li.classList.add('collapsibleList');
				}else {
					li.classList.add('uncollapsibleList');
				}
			}
			root.addEventListener('click', 
				function(e) {
					if (!e) e = window.event;
					var li = (e.target ? e.target : e.srcElement);
					if ((li.nodeName == 'LI') && (li.classList.contains("collapsibleList")) && (e.offsetX < 0)) li.classList.toggle("Closed");
				},
				false);
		};
	};

	document.addEventListener("DOMContentLoaded", function() {
		var tocDiv = document.getElementById("toc"),
			mainDiv = document.getElementById("blockrightcol"),
			tocRoot = tocDiv.children[1],
			expandAll = document.createElement("DIV");
		var hideBtn = document.createElement("DIV");
		hideBtn.setAttribute('id', 'toc-hide-block');
		hideBtn.setAttribute('hideblock-id', 'toc');
		hideBtn.classList.add('hideBtn', 'toHide');
		mainDiv.insertBefore(hideBtn, mainDiv.childNodes[0]);
		mainDiv.classList.add("width_part");
		expandAll.classList.add("collapseAll");
		tocDiv.insertBefore(expandAll, tocRoot);

		mainDiv.addEventListener('click',
			function(e) {
				if (!e) e = window.event;
				var th = (e.target ? e.target : e.srcElement);
				if (th.nodeName == 'TH') {
					var table = th;
					while (table.nodeName != 'TABLE') table = table.parentNode;
					if (table.classList.contains("sortable")) {
						sort(table, th, !e.ctrlKey);
					}
				}
			},
			false);

		var hidefunc = function() { 
			var divToHide = document.getElementById(this.getAttribute("hideblock-id"));
			if (divToHide == undefined) { return; }
			if (this.classList.contains("toShow")) {
				divToHide.style.display = "block";
				this.classList.remove("toShow");
				this.classList.add("toHide");
			} else {
				divToHide.style.display = "none";
				this.classList.remove("toHide");
				this.classList.add("toShow");
			}
		}
		document.getElementById("toc-hide-block").onclick = hidefunc;

		var cl = new CollapsibleLists;
		cl.init(tocRoot);

		expandAll.addEventListener('click', 
			function(e) {
				expandAll.classList.toggle('collapsed');
				cl.expand(expandAll.classList.contains('collapsed'));
			},
			false);
		expandAll.dispatchEvent(new Event('click'));
	});
})();
