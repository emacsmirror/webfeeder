(require 'ert)

(add-to-list 'load-path ".")
(require 'webfeeder)
(require 'diff)
(require 'cl-lib)

(defvar webfeeder-test-dir "testdata")

(defun webfeeder--file-to-string (file)
  "Return the file content as string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun webfeeder--string=-explainer (string-a string-b)
  "Return the diff output of STRING-A and STRING-B"
  (unless (string= string-a string-b)
    (let (file-a file-b)
      (unwind-protect
          (let (result)
            (setq file-a (make-temp-file "webfeeder")
                  file-b (make-temp-file "webfeeder"))
            (with-temp-file file-a
              (insert string-a))
            (with-temp-file file-b
              (insert string-b))
            (setq result
                  (with-temp-buffer
                    ;; The following generates a *Diff* buffer which is
                    ;; convenient for coloration.
                    (diff file-a file-b nil 'no-async)
                    (diff-no-select file-a file-b nil 'no-async (current-buffer))
                    (buffer-string)))
            result)
        (delete-file file-a)
        (delete-file file-b)))))

(put 'string= 'ert-explainer 'webfeeder--string=-explainer)

(defun webfeeder--test-pages (feed html-files
                                     &optional builder max-entries)
  (setq builder (or builder 'webfeeder-make-atom))
  (let* ((feed-buffer (webfeeder--file-to-string feed))
         (feed-items (webfeeder-html-files-to-items "." "https://example.org/"
                                                       html-files)))
    (should
     (string=
      feed-buffer
      (funcall builder feed "https://example.org/" feed-items
               :title "Example feed"
               :description "Example description"
               :build-date 0
               :max-entries max-entries)))
    (should-not
     (string=
      feed-buffer
      (funcall builder feed "https://example.org/" feed-items
                                 :title "Example TYPO"
                                 :description "Example description"
                                 :build-date 0
                                 :max-entries max-entries)))
    (should-not
     (string=
      feed-buffer
      (funcall builder feed "https://example.XYZ/" feed-items
                                 :title "Example feed"
                                 :description "Example description"
                                 :build-date 0
                                 :max-entries max-entries)))
    (should-not
     (string=
      feed-buffer
      (funcall builder feed "https://example.org/" feed-items
                                 :title "Example feed"
                                 :description "Example description TYPO"
                                 :build-date 0
                                 :max-entries max-entries)))))

(ert-deftest webfeeder-single-rss-libxml ()
  "Simple test using libxml backend.
This requires an Emacs compiled against libxml."
  (cl-assert (file-directory-p webfeeder-test-dir))
  (let ((default-directory (expand-file-name webfeeder-test-dir default-directory)))
    (let ((webfeeder-date-function 'webfeeder-date-libxml)
          (webfeeder-title-function 'webfeeder-title-libxml)
          (webfeeder-body-function 'webfeeder-body-libxml))
      (webfeeder--test-pages "libxml-post0.rss" '("post0-html5-fancy.html")
                               'webfeeder-make-rss))))

(ert-deftest webfeeder-single-rss-default ()
  "Simple test using regular expressions to parse XML."
  (cl-assert (file-directory-p webfeeder-test-dir))
  (let ((default-directory (expand-file-name webfeeder-test-dir default-directory)))
    (let ((webfeeder-date-function 'webfeeder-date-default)
          (webfeeder-title-function 'webfeeder-title-default)
          (webfeeder-body-function 'webfeeder-body-default))
      (webfeeder--test-pages "default-post0.rss" '("post0-html5-fancy.html")
                               'webfeeder-make-rss))))

(ert-deftest webfeeder-multi-rss-libxml ()
  (cl-assert (file-directory-p webfeeder-test-dir))
  (let ((default-directory (expand-file-name webfeeder-test-dir default-directory)))
    (let ((webfeeder-date-function 'webfeeder-date-libxml)
          (webfeeder-title-function 'webfeeder-title-libxml)
          (webfeeder-body-function 'webfeeder-body-libxml))
      (webfeeder--test-pages "libxml-post0+post1.rss" '("post0-html5-fancy.html"
                                                          "post1-html5-fancy.html")
                               'webfeeder-make-rss))))

(ert-deftest webfeeder-limited-multi-rss-libxml ()
  (cl-assert (file-directory-p webfeeder-test-dir))
  (let ((default-directory (expand-file-name webfeeder-test-dir default-directory)))
    (let ((webfeeder-date-function 'webfeeder-date-libxml)
          (webfeeder-title-function 'webfeeder-title-libxml)
          (webfeeder-body-function 'webfeeder-body-libxml))
      (webfeeder--test-pages "libxml-post1.rss" '("post0-html5-fancy.html"
                                                    "post1-html5-fancy.html")
                               'webfeeder-make-rss 1))))

(ert-deftest webfeeder-single-rss-no-html5 ()
  (cl-assert (file-directory-p webfeeder-test-dir))
  (let ((default-directory (expand-file-name webfeeder-test-dir default-directory)))
    (let ((webfeeder-date-function 'webfeeder-date-libxml)
          (webfeeder-title-function 'webfeeder-title-libxml)
          (webfeeder-body-function 'webfeeder-body-libxml))
      (webfeeder--test-pages "libxml-post0-no-html.rss" '("post0.html")
                               'webfeeder-make-rss))))

(ert-deftest webfeeder-single-rss-no-fancy ()
  (cl-assert (file-directory-p webfeeder-test-dir))
  (let ((default-directory (expand-file-name webfeeder-test-dir default-directory)))
    (let ((webfeeder-date-function 'webfeeder-date-libxml)
          (webfeeder-title-function 'webfeeder-title-libxml)
          (webfeeder-body-function 'webfeeder-body-libxml))
      (webfeeder--test-pages "libxml-post0-no-fancy.rss" '("post0-html5.html")
                               'webfeeder-make-rss))))

;; Atom tests.
(ert-deftest webfeeder-single-atom-libxml ()
  (cl-assert (file-directory-p webfeeder-test-dir))
  (let ((default-directory (expand-file-name webfeeder-test-dir default-directory)))
    (let ((webfeeder-date-function 'webfeeder-date-libxml)
          (webfeeder-title-function 'webfeeder-title-libxml)
          (webfeeder-body-function 'webfeeder-body-libxml))
      (webfeeder--test-pages "libxml-post0.atom" '("post0-html5-fancy.html")
                                     'webfeeder-make-atom))))

(ert-deftest webfeeder-single-atom-default ()
  (cl-assert (file-directory-p webfeeder-test-dir))
  (let ((default-directory (expand-file-name webfeeder-test-dir default-directory)))
    (let ((webfeeder-date-function 'webfeeder-date-default)
          (webfeeder-title-function 'webfeeder-title-default)
          (webfeeder-body-function 'webfeeder-body-default))
      (webfeeder--test-pages "default-post0.atom" '("post0-html5-fancy.html")
                                     'webfeeder-make-atom))))

(ert-deftest webfeeder-multi-atom-libxml ()
  (cl-assert (file-directory-p webfeeder-test-dir))
  (let ((default-directory (expand-file-name webfeeder-test-dir default-directory)))
    (let ((webfeeder-date-function 'webfeeder-date-libxml)
          (webfeeder-title-function 'webfeeder-title-libxml)
          (webfeeder-body-function 'webfeeder-body-libxml))
      (webfeeder--test-pages "libxml-post0+post1.atom" '("post0-html5-fancy.html"
                                                                "post1-html5-fancy.html")
                                     'webfeeder-make-atom))))

(ert-deftest webfeeder-limited-multi-atom-libxml ()
  (cl-assert (file-directory-p webfeeder-test-dir))
  (let ((default-directory (expand-file-name webfeeder-test-dir default-directory)))
    (let ((webfeeder-date-function 'webfeeder-date-libxml)
          (webfeeder-title-function 'webfeeder-title-libxml)
          (webfeeder-body-function 'webfeeder-body-libxml))
      (webfeeder--test-pages "libxml-post1.atom" '("post0-html5-fancy.html"
                                                    "post1-html5-fancy.html")
                               'webfeeder-make-atom 1))))

(ert-deftest webfeeder-single-atom-no-html5 ()
  (cl-assert (file-directory-p webfeeder-test-dir))
  (let ((default-directory (expand-file-name webfeeder-test-dir default-directory)))
    (let ((webfeeder-date-function 'webfeeder-date-libxml)
          (webfeeder-title-function 'webfeeder-title-libxml)
          (webfeeder-body-function 'webfeeder-body-libxml))
      (webfeeder--test-pages "libxml-post0-no-html.atom" '("post0.html")
                                     'webfeeder-make-atom))))

(ert-deftest webfeeder-single-atom-no-fancy ()
  (cl-assert (file-directory-p webfeeder-test-dir))
  (let ((default-directory (expand-file-name webfeeder-test-dir default-directory)))
    (let ((webfeeder-date-function 'webfeeder-date-libxml)
          (webfeeder-title-function 'webfeeder-title-libxml)
          (webfeeder-body-function 'webfeeder-body-libxml))
      (webfeeder--test-pages "libxml-post0-no-fancy.atom" '("post0-html5.html")
                                     'webfeeder-make-atom))))

(provide 'webfeeder-test)
