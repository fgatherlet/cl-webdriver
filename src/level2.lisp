(in-package :webdriver)

;; utilities

(defun wd-wait-until% (test-fn &key (time 10) (sleep-sec 0.5))
  (collect-first (choose
                  (mapping ((i (scan-range :length time)))
                    (if-let ((ret (funcall test-fn)))
                      ret
                      (sleep sleep-sec))))))

(defmacro wd-wait-until ((&key (time 80) (sleep-sec 0.1)) &body body)
  `(wd-wait-until%
    (lambda () ,@body)
    ,@(when time `(:time ,time))
    ,@(when sleep-sec `(:sleep-sec ,sleep-sec))))

(defun wd-google (q)
  (setf (wd-url) (format nil "https://www.google.com/search?q=~a" q)))

(defun wd-make-window ()
  (let ((windows (wd-all-windows))
        diff)
    (wd-execute "window.open()")
    (setq diff (set-difference (wd-all-windows) windows :test #'string=))
    (if (and diff (= 1 (length diff)))
        (setf (wd-window) (car diff))
      (error "can not create window"))))

(defparameter inject-util "
function xpath(xpathq) {
  var headings = document.evaluate(xpathq, document, null, XPathResult.ANY_TYPE, null );
  var node = headings.iterateNext();
  var result = [];
  while (node) {
    result.push(node);
    node = headings.iterateNext();
  }
  return result;
}
")
(defparameter inject-disable-tooltip "
xpath('//div[@xtype=\\'xtooltip\\']').forEach((elm)=>{
  elm.parentElement.removeChild(elm);
});
")

(defun wd-disable-tooltip ()
  (wd-execute (concatenate
               'string
               inject-util
               inject-disable-tooltip
               )))

(defun wd-enable-tooltip ()
  (wd-execute 
   (concatenate
    'string
    inject-util
    inject-disable-tooltip
    "
var elms = xpath('//a');
var i = 0;
elms.forEach(elm=>{
  elm.setAttribute('xid', `${i}`);
  //elm.textContent = `${i}-${elm.textContent}`;
  var tt = document.createElement('div');
  tt.style['display'] = 'block';
  tt.style['position'] = 'absolute';
  tt.style['bottom'] = '-2.8em';
  tt.style['left'] = '0.3em';
  tt.style['z-index'] = '9999';
  tt.style['width'] =  'auto';
  tt.style['height'] =  'auto';
  tt.style['padding'] =  '0.3em 0.5em';
  tt.style['color'] =  '#FFFFFF';
  tt.style['background'] =  '#c72439';
  tt.style['border-radius'] =  '0.5em';
  tt.style[''] = '';
  tt.style[''] = '';
  tt.style[''] = '';
  tt.setAttribute('xtype', 'xtooltip');
  tt.innerHTML = `${i}`;
  elm.appendChild(tt);
  i = i + 1;
});


function xref(id) {
  var elm = xpath(`//a[@xid = ${id}]`)[0];
  return elm;
}
")))

(defun wd-click-tooltip (i)
  (wd-element-click (wd-find (format nil "//a[@xid = ~a]" i))))

(defun wd-page-up ()
  (wd-actions (:keys :page-up)))
(defun wd-page-down ()
  (wd-actions (:keys :page-down)))


#|
https://unguis.cre8or.jp/web/1934
.sample5-tooltips {
	display: block;
	position: absolute;
	bottom: -2.8em;
	left: 0.3em;
	z-index: 9999;
	width: auto;
	height: auto;
	padding: 0.3em 0.5em;
	color: #FFFFFF;
	background: #c72439;
	border-radius: 0.5em;
}
.sample5-tooltips:after {
	width: 100%;
	content: "";
	display: block;
	position: absolute;
	left: 0.5em;
	top: -8px;
	border-top:8px solid transparent;
	border-left:8px solid #c72439;
}
|#
