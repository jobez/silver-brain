(in-package silver-brain/tests.server)

(defvar *software* (make-instance 'concept
                                  :name "Software"
                                  :content "Software Content"))

(defvar *emacs* (make-instance 'concept
                               :name "Emacs"
                               :content "Emacs Content"))

(defvar *port* 15000
  "Port to use.")

(defun url (format-string &rest args)
  (apply #'format
         (append
          (list nil (concatenate 'string
                                 "http://localhost:"
                                 (format nil "~a" *port*)
                                 format-string))
          args)))

(defun setup-test-server ()
  (let* ((concept-map (make-instance 'concept-map)))
    (become-child *software* *emacs*)
    (add-concept concept-map *software*)
    (add-concept concept-map *emacs*)
    (setup-server concept-map)))

(setup
  (start-server :port *port* :debug t)
  (format t "Waiting 0.5 second for server to start...~&")
  (sleep 0.5))

(teardown
  (stop-server))

(defhook :before
  (setup-test-server))

(deftest test-get-concepts
  (testing "GET /concepts/"
    (let ((result (decode-json-from-string
                   (dex:get (url "/concepts/") :keep-alive nil))))
      (ok (= (length result) 2)
          "Returns 2 results.")
      (ok (member (concept-id *software*)
                  (mapcar (lambda (alist) (assoc-value alist :id)) result)
                  :test #'string=)
          "Contains correct concept."))))

(deftest test-post-concepts
  (testing "POST /concepts/"
    (match (multiple-value-list
            (dex:post (url "/concepts/")
                          :content (encode-json-to-string
                                    '((:name . "Vim")
                                      (:content . "Content Vim")))
                          :keep-alive nil))
      ((list _ code headers _ _)
       (ok (= code 201)
           "Returns 201.")
       (ok (gethash "location" headers)
           "Location header is set.")))))

(deftest test-get-concept-by-id
  (testing "GET /concepts/:id"
    (let ((result (decode-json-from-string
                   (dex:get (url "/concepts/~a" (concept-id *software*))))))
      (ok (string= (assoc-value result :id) (concept-id *software*))))
    (ok (signals
            (dex:get (url "/concepts/1234"))
            'dex:http-request-not-found)
        "Returns 404 when :id is wrong.")))

(deftest test-put-concept-by-id
  (testing "PUT /concepts/:id"
    (ok (signals
            (dex:put (url "/concepts/1234"))
            'dex:http-request-not-found)
        "Returns 404 when :id is wrong.")
    (ok (signals
            (dex:put (url "/concepts/~a" (concept-id *software*)))
            'dex:http-request-bad-request)
        "Returns 400 when no content is given.")))

(deftest test-delete-concept-id
  (testing "DELETE /concepts/:id"
    (ok (dex:delete (url "/concepts/~a" (concept-id *software*)))
        "DELETE succeeded."))
  (testing "Delete wrong concept"
    (ok (signals
            (dex:delete (url "/concepts/1234"))
            'dex:http-request-not-found)
        "Returns 404 when :id is invalid.")))

;; (start-server)
;; (let ((*port* 5000))
;;   (setup-test-server)
;;   (run-test 'test-get-concept-by-id)
;;   )
