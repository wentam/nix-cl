(require "asdf")

(defpackage :lib
            (:use :cl)
            (:export #:deps-of
                     #:asd-path
                     #:path-is-child
                     #:deps-if-internal
                     #:resolve-conditional-dep
                     #:remove-internal-deps
                     #:path-deps
                     #:external-path-deps
                     #:internal-path-deps
                     #:system-version
                     #:asdf-escaped-system-name
                     #:bundle-fast-load-extension
                     #:fast-load-extension
                     #:monolithic-bundle-filename
                     #:find-monolithic-bundle))

;; Lookup tables
(defvar *impl-extension-map* `(
  (SBCL . "fasl")
  (ECL  . "fas" )
  (CCL  . "lx64fsl")
))

(defvar *impl-bundle-extension-map* `(
  (SBCL . "fasl")
  (ECL  . "fasb" )
  (CCL  . "lx64fsl")
))

(in-package :lib)

;; (:FEATURE :CORMAN (:REQUIRE "threads"))

;; (:FEATURE (:AND (:OR :SBCL :ECL) (:NOT :USOCKET-IOLIB)) :SB-BSD-SOCKETS)

(defun resolve-conditional-dep (dep)
  (when (atom dep)
    (return-from resolve-conditional-dep (string-downcase dep)))

  (when (and (equal (first dep) :FEATURE)
             (uiop:featurep (second dep)))
    (return-from resolve-conditional-dep
      (resolve-conditional-dep (third dep))))

  (when (and (equal (first dep) :VERSION))
    (return-from resolve-conditional-dep
      (resolve-conditional-dep (second dep))))

  (when (and (equal (first dep) :REQUIRE))
    (return-from resolve-conditional-dep
      (resolve-conditional-dep (second dep)))))


(defun deps-of (s)
  "Direct dependencies of an ASDF system, conditional dependencies resolved to strings as relevant"
  (remove nil
    (mapcar #'(lambda (x)(resolve-conditional-dep x))
      (asdf:component-sideway-dependencies (asdf:find-system s)))))

(defun asd-path(system-name)
  "Path to the asd file for the given ASDF system name. If it can't be found, returns '/dev/null'."
  (truename
    (or (ignore-errors (asdf:system-source-file (asdf:find-system system-name)))
        #P"/dev/null")))

(defun path-is-child (potential-child parent)
  "T if potential-child is a child path of parent, direct or indirect, nil otherwise"
  (not (eq :absolute (car (pathname-directory (enough-namestring (truename potential-child)
                                                                 (truename parent)))))))

(defun deps-if-internal (system system-source-path)
  "If it's a system internal to the source path: direct dependencies of the system. Else: nil"
  (when (path-is-child (pathname (asd-path system)) (pathname system-source-path))
    (deps-of system)))

(defun remove-internal-deps (deps system-source-path)
  "Given a list of ASDF system names, removes the systems who are defined in system-source-path."
  (remove-if #'(lambda (dep)
                 (path-is-child (pathname (asd-path dep)) (pathname system-source-path)))
             deps))

(defun remove-external-deps (deps system-source-path)
  "Given a list of ASDF system names, removes the systems who are not defined in system-source-path."
  (remove-if-not #'(lambda (dep)
                     (path-is-child (pathname (asd-path dep)) (pathname system-source-path)))
                 deps))

(defun path-deps (system system-source-path)
  "Traverses the dependency tree for a given system, returning only systems who are direct
  dependencies of the systems in the provided path"
  (let ((ret (deps-if-internal system system-source-path))
        (child-deps nil))

    (loop for dep in ret do
      (setf child-deps (concatenate 'list (path-deps dep system-source-path) child-deps)))

    (mapcar #'asdf:coerce-name (remove-duplicates (concatenate 'list ret child-deps) :test #'equal))))

(defun external-path-deps (system system-source-path)
  "Like path-deps, but only returns dependencies that are external to the path"
  (remove-internal-deps (path-deps system system-source-path) system-source-path))

(defun internal-path-deps (system system-source-path)
  "Like path-deps, but only returns dependencies that are internal to the path"
  (remove-external-deps (path-deps system system-source-path) system-source-path))

(defun system-version (system-name)
  "The version of an ASDF system"
  (let ((system (asdf:find-system system-name nil)))
    (when (and system (slot-boundp system 'asdf:version))
      (asdf:component-version system))))

(defun replace-char-with-str (c str input-str)
  "Non-destructively replaces every instance of c with str in input-str and returns the result"
  (with-output-to-string (s)
     (loop for ch across input-str do
       (if (char= ch c)
         (princ str s)
         (princ ch s)))))

(defun asdf-escaped-system-name (system-name)
  (replace-char-with-str #\/ "--" system-name))

(defmacro fast-load-extension (impl)
  "The file extension for fast-load files given implementation name (symbol or string)"
  (if (symbolp (eval impl))
    `(cdr (assoc ,impl cl-user::*impl-extension-map*))
    `(cdr (assoc (read-from-string ,impl) cl-user::*impl-extension-map*))))

(defmacro bundle-fast-load-extension (impl)
  "The file extension for bundled fast-load files given implementation name (symbol or string)"
  (if (symbolp (eval impl))
    `(cdr (assoc ,impl cl-user::*impl-bundle-extension-map*))
    `(cdr (assoc (read-from-string ,impl) cl-user::*impl-bundle-extension-map*))))

(defmacro monolithic-bundle-filename (impl system-name &key noext)
  "Name of the file - with implementation-specific extension - used for ASDF monolithic bundles for
  a given system."
  (if (eval noext)
    `(format nil
            "~a--all-systems"
            (asdf-escaped-system-name ,system-name))
    `(format nil
            "~a--all-systems.~a"
            (asdf-escaped-system-name ,system-name)
            (bundle-fast-load-extension ,impl))))

(defmacro find-monolithic-bundle (path impl system-name)
  "Tries to recursively find a monolithic-built fasl in a given path. Nil if not found."
  `(car (uiop:directory*
         (concatenate 'string
                      ,path
                      "/**/"
                      (monolithic-bundle-filename ,impl ,system-name)))))
