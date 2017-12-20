;;; company-javadoc-lookup.el --- company-mode completion backend for javadoc-lookup
;;
;;; Metadata
;; Copyright (C) 2017 Andreas Marschke
;;
;; Author: Andreas Marschke
;; Created: 1 May 2017
;; Version: 0.1
;; Keywords: java, javadoc, company, completion
;; URL:
;;; Changelog
;;
;;;; Version 0.1 - Initial Release
;;
;; - supports basic lookup based on javadoc-lookup
;;
;;; Commentary:
;;
;; A company-mode backend adding support for javadoc-lookup where we are resolving based
;; on classes accessible via `jdl/get-class-list'
;;
;;
;;; Usage:
;; In your configuration you may add this backend in a mode-specific setup ie.:
;;
;; (add-hook 'java-mode-hook
;;           (lambda ()
;;             (add-to-list (make-local-variable 'company-backends)
;;                          'company-javadoc-lookup))
;;
;;
;;; Code:

;;;; Requirements
(require 'company)
(require 'cl-lib)
(require 'javadoc-lookup)

;;;; Vars
(defvar company-jdl/-java-import-tree nil
  "Internal tree structure containing all import list nodes")

;;;; Class definition
(defclass company-jdl-node()
  ((children :initarg :children
	     :label "children"
             :initform (list)
	     :documentation "Children of a node")
   (value :initarg :value
	  :label "value"
	  :documentation "Value of node")))

;;;; Utility Functions
(defun company-jdl/build-node (value)
  "Builds new node based on VALUE"
  (company-jdl-node :value value)
  )

(defun company-jdl/node-list-get-values (node-list)
  "Get values for all elements in NODE-LIST"
  (cl-map 'list '(lambda (node) (oref node value)) node-list))

(defun company-jdl/has-children (node)
  "Return non-nil if NODE's :children has values"
  (not (eq nil (ignore-errors (oref node children)))))

(defun company-jdl/has-named-child (node name)
  "Checks if one of the children defined in NODE matches NAME"
  (cl-loop for child in (ignore-errors (oref node children)) do
	   (if (string-match (oref child value) name)
	       (cl-return child))))

(defun company-jdl/get-named-child (node name)
  (company-jdl/has-named-child node name))

(defun company-jdl/node-matches-value (node name)
  (string-match (oref node value) name))

(defun company-jdl/find-deepest-match (node-root name-list)
  "Iterate down the trees of NODE-LIST to find the deepest NODE matching the full
 chain of NAME-LIST. Returns last found node via NODE-CURRENT."
  (cl-labels
      ((find-deepest-match
        (-p-node-root -p-name-list)
        (cond ((and (eq nil (ignore-errors (oref -p-node-root children))) (<= 1 (length -p-name-list))) -p-node-root)
              ((= 1 (length -p-name-list)) (company-jdl/get-named-child -p-node-root (car -p-name-list)))
              ((< 1 (length -p-name-list)) (find-deepest-match (company-jdl/get-named-child -p-node-root (car -p-name-list)) (cdr -p-name-list)))
	      )
	))
    (find-deepest-match node-root name-list))
  )

(defun company-jdl/match-regular-expression (regex content num)
  (string-match regex content)
  (match-string-no-properties num content))

(defun company-jdl/find-package-node-list-from-root (content)
  (let ((-token-list (seq-filter (lambda (elt) (not (string-equal elt ""))) (split-string content "\\."))))
    (oref
     (company-jdl/find-deepest-match
      company-jdl/-java-import-tree
      -token-list)
     children)
    )
  )

;;;; Handle arg 
(defun company-jdl/candidates ()
  (looking-back "^import \\(.*\\)")
  (let ((content (match-string-no-properties 1)))
    (message (format "Content is: '%s'" content))
    (cond ((and (not (eq nil content)) (not (string-equal content "")))
           (company-jdl/node-list-get-values (company-jdl/find-package-node-list-from-root content)))
          ((or (eq nil content) (string-equal "" content))
           (company-jdl/node-list-get-values (oref company-jdl/-java-import-tree children)))
          )))

;;;; Compile Tree Function
(defun company-jdl/build-tree (full-path index tree)
  (if (> index (- (length (split-string full-path "\\.")) 1))
      nil
    (let ((-tokenized-path (split-string full-path "\\.")))
      (let ((-at-index (nth index -tokenized-path)))
        (cond
         ((not (company-jdl/has-named-child tree -at-index))
          (let ((new-index (1+ index))
                (new-node (company-jdl/build-node -at-index)))
            (object-add-to-list tree :children new-node)
            (company-jdl/build-tree full-path new-index new-node)) )
         
         ((company-jdl/has-named-child tree -at-index)
          (let ((new-index (1+ index)))
            (if (< new-index (length -tokenized-path))
                (company-jdl/build-tree full-path new-index (company-jdl/get-named-child tree -at-index)))) )
         )
        )
      )
    )
  )

(defun compile-tree ()
  (setq company-jdl/-java-import-tree (company-jdl/build-node ""))
  (cl-loop for class in (jdl/get-class-list)  do
           (company-jdl/build-tree class 0 company-jdl/-java-import-tree))
  company-jdl/-java-import-tree
  )

(compile-tree)

;;;; Company Mode backend

;;;###autoload
(defun company-javadoc-lookup (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive
     (company-begin-backend 'company-javadoc-lookup))
    (prefix
     (and (eq major-mode 'java-mode)
          buffer-file-name
          (not (company-in-string-or-comment))
          (company-grab-symbol-cons "^import \\(.*\\)$")
          ))
    (candidates (company-jdl/candidates))
    (sorted t)
    (no-cache nil)))

(provide 'company-javadoc-lookup)
