;;;; specs/runner.lisp
;;;;
;;;; A direct runner for this bundle: it checks every Function Spec and
;;;; Property through cl-spec's Lisp API under fixed seeds and judges the
;;;; results strictly.  scripts/check-specs.lisp is its command-line entry and
;;;; what CI runs.

(in-package :wardlisp/specs)

(defparameter *default-seeds* '(1 42 20261012)
  "Seeds every target runs under.  Fixed, so a CI failure replays locally.")

(defparameter *default-profile* :normal
  "Trial profile every Property runs under.")

(defparameter *default-trials* 200
  "Trial budget every Function Spec runs under.")

(defun contract-names ()
  "Return the targets of this bundle's Function Specs."
  '(evaluate print-value make-string-value string-value
    string-value-p symbol-value-p number-value-p))

(defun property-names ()
  "Return the names of this bundle's Properties."
  '(evaluate-reports-invalid-arguments
    evaluate-default-fuel-is-documented
    evaluate-sees-host-bindings
    evaluate-is-deterministic
    step-limit-is-exact
    memory-limit-is-exact
    recursion-limit-is-exact
    output-limit-is-exact
    integer-arithmetic-agrees-with-common-lisp
    out-of-range-float-literals-are-parse-errors
    float-overflow-is-a-number-limit
    quotient-and-mod-truncate-toward-zero
    quoted-data-round-trip-through-print-value
    equal?-is-structural-equality
    value-classifiers-partition-data
    host-strings-are-copied-both-ways
    evaluate-results-carry-host-strings))

;;; --- Running one target ---

(defun result-record (result)
  "Return what the runner keeps of a cl-spec RESULT, read from its version 1
RESULT-DATA record."
  (let ((data (cl-spec:result-data result)))
    (if (not (eql 1 (getf data :schema-version)))
        (list :status :unsupported-result :schema-version (getf data :schema-version))
        (list :status (getf data :status)
              :trials (getf data :trials)
              :rejected (getf data :rejected)
              :digest (getf data :definition-digest)
              :case-report (getf data :case-report)
              :counterexample (getf data :counterexample)
              :shrunk-counterexample (getf data :shrunk-counterexample)
              :failure-phase (getf data :failure-phase)
              :failure-reason (getf data :failure-reason)
              :condition (let ((condition (cl-spec:property-result-condition result)))
                           (and condition (princ-to-string condition)))))))

(defun declared-cases (name)
  "Return the case names the Function Spec NAME declares, in order."
  (mapcar (lambda (case) (getf case :name))
          (getf (cl-spec:function-spec-data name) :cases)))

(defun run-target (kind name seed &key (profile *default-profile*)
                                       (trials *default-trials*))
  "Run the target NAME of KIND (:FUNCTION-SPEC or :PROPERTY) under SEED and
return its entry plist.  A condition the run signals becomes the entry's
:SIGNALLED status instead of escaping."
  (let ((base (list :kind kind :name name :seed seed)))
    (handler-case
        (ecase kind
          (:function-spec
           (append base
                   (list :declared-cases (declared-cases name))
                   (result-record (cl-spec:check-function name :trials trials :seed seed))))
          (:property
           (append base
                   (result-record (cl-spec:run-property name :profile profile :seed seed)))))
      (error (condition)
        (append base (list :status :signalled
                           :condition (princ-to-string condition)))))))

;;; --- Judging ---

(defun case-problems (declared report)
  "Return the problems of a contract declaring the cases DECLARED, given the
case REPORT of its run: an unmeasured report, a failed selection or capture,
or a declared case no trial reached."
  (if (not (consp report))
      (list (list :case-report-unmeasured report))
      (append
       (unless (eql 0 (getf report :case-selection-errors))
         (list (list :case-selection-errors (getf report :case-selection-errors))))
       (unless (eql 0 (getf report :capture-errors))
         (list (list :capture-errors (getf report :capture-errors))))
       (when (getf report :never-called)
         (list (list :cases-never-called (getf report :never-called))))
       (loop for case in declared
             for called = (getf (find case (getf report :cases)
                                      :key (lambda (row) (getf row :name)))
                                :called)
             unless (and (integerp called) (plusp called))
               collect (list :case-not-called case)))))

(defun judge-entry (entry)
  "Return the problems of ENTRY; NIL means it passed.  Only :PASSED passes, and
only with a positive trial count, a measured rejection count below it, and --
for a contract with cases -- every declared case reached."
  (let ((status (getf entry :status))
        (trials (getf entry :trials))
        (rejected (getf entry :rejected))
        (declared (getf entry :declared-cases)))
    (cond ((not (eq status :passed))
           (list (list :status status)))
          ((not (and (integerp trials) (plusp trials)))
           (list (list :no-trials trials)))
          ((and (eq (getf entry :kind) :function-spec)
                (not (and (integerp rejected) (<= 0 rejected) (< rejected trials))))
           (list (list :rejections trials rejected)))
          (declared
           (case-problems declared (getf entry :case-report)))
          (t nil))))

(defun bundle-targets ()
  "Return every target of this bundle as (KIND NAME)."
  (append (mapcar (lambda (name) (list :function-spec name)) (contract-names))
          (mapcar (lambda (name) (list :property name)) (property-names))))

(defun registration-problems ()
  "Return the bundle's targets that are not registered, and the registered
definitions of this package that the bundle does not list."
  (let ((package (find-package :wardlisp/specs)))
    (flet ((ours (names) (remove-if-not (lambda (name) (eq (symbol-package name) package))
                                        names)))
      (append
       (loop for name in (contract-names)
             unless (nth-value 1 (cl-spec:find-function-spec name))
               collect (list :function-spec-not-registered name))
       (loop for name in (property-names)
             unless (nth-value 1 (cl-spec:find-property name))
               collect (list :property-not-registered name))
       (loop for name in (ours (cl-spec:list-properties))
             unless (member name (property-names))
               collect (list :property-not-listed name))))))

(defun run-bundle (&key (targets (bundle-targets)) (seeds *default-seeds*)
                        (profile *default-profile*) (trials *default-trials*))
  "Run each of TARGETS under each of SEEDS and return a report plist whose :OK is
true only when every entry passed and the registry matches the bundle."
  (let* ((entries (loop for seed in seeds
                        append (loop for (kind name) in targets
                                     collect (run-target kind name seed
                                                         :profile profile :trials trials))))
         (problems (append (registration-problems)
                           (loop for entry in entries
                                 for found = (judge-entry entry)
                                 when found
                                   collect (list (getf entry :kind) (getf entry :name)
                                                 :seed (getf entry :seed)
                                                 :problems found)))))
    (list :ok (null problems)
          :problems problems
          :selection (list :seeds seeds :profile profile :trials trials)
          :entries entries)))

;;; --- Negative control ---

(defun mutant-print-wstring (ws)
  "PRINT-WSTRING that forgets to escape."
  (format nil "\"~A\"" (wstring-value ws)))

(defun mutant-make-string-value (string)
  "MAKE-STRING-VALUE that shares the host's storage."
  (unless (stringp string)
    (error 'wardlisp-type-error :message "make-string-value: expected a CL string"))
  (wardlisp/src/types:make-wstring string))

(defun mutant-consume-fuel (ctx &optional (amount 1))
  "CONSUME-FUEL that stops one step early, when fuel reaches zero."
  (decf (wardlisp/src/types:exec-ctx-fuel ctx) amount)
  (incf (wardlisp/src/types:exec-ctx-steps-used ctx) amount)
  (when (<= (wardlisp/src/types:exec-ctx-fuel ctx) 0)
    (error 'wardlisp/src/types:wardlisp-step-limit-exceeded :message "mutant")))

(defun mutant-track-cons (ctx &optional (count 1))
  "TRACK-CONS that refuses the allocation reaching the limit."
  (incf (wardlisp/src/types:exec-ctx-cons-count ctx) count)
  (when (>= (wardlisp/src/types:exec-ctx-cons-count ctx)
            (wardlisp/src/types:exec-ctx-max-cons ctx))
    (error 'wardlisp/src/types:wardlisp-memory-limit-exceeded :message "mutant")))

(defun mutant-builtin-mod (args ctx)
  "The mod builtin computed with CL:MOD, which floors instead of truncating."
  (declare (ignore ctx))
  (when (zerop (second args))
    (error 'wardlisp-type-error :message "mod: division by zero"))
  (mod (first args) (second args)))

(defun mutant-parse-float-token (token)
  "PARSE-FLOAT-TOKEN that hands the token to the host reader unguarded, as
before out-of-range literals were classified."
  (let ((*read-default-float-format* 'double-float)
        (*read-eval* nil))
    (read-from-string token)))

(defun mutant-call-checking-float-range (name thunk)
  "CALL-CHECKING-FLOAT-RANGE that lets a float overflow escape to the host."
  (declare (ignore name))
  (funcall thunk))

(defparameter *mutants*
  '((wardlisp/src/builtins::print-wstring mutant-print-wstring
     (:function-spec print-value) (:property quoted-data-round-trip-through-print-value))
    (wardlisp:make-string-value mutant-make-string-value
     (:function-spec make-string-value) (:property host-strings-are-copied-both-ways))
    (wardlisp/src/types:consume-fuel mutant-consume-fuel
     (:property step-limit-is-exact))
    (wardlisp/src/types:track-cons mutant-track-cons
     (:property memory-limit-is-exact))
    (wardlisp/src/builtins::builtin-mod mutant-builtin-mod
     (:property quotient-and-mod-truncate-toward-zero))
    (wardlisp/src/reader::parse-float-token mutant-parse-float-token
     (:property out-of-range-float-literals-are-parse-errors))
    (wardlisp/src/builtins::call-checking-float-range mutant-call-checking-float-range
     (:property float-overflow-is-a-number-limit)))
  "Each entry is (FUNCTION MUTANT . TARGETS): a wrong implementation of
FUNCTION, and the targets that must catch it.")

(defun call-with-mutant (function mutant thunk)
  "Call THUNK with FUNCTION's global definition replaced by MUTANT's, restoring
it afterwards."
  (let ((original (fdefinition function)))
    (unwind-protect
         (progn (setf (fdefinition function) (fdefinition mutant))
                (funcall thunk))
      (setf (fdefinition function) original))))

(defun run-negative-control (&key (seeds *default-seeds*))
  "Run each mutant's targets with the mutant in place and return a report whose
:OK is true only when every target caught its mutant under every seed: a
contract that passes a known-wrong implementation is not evidence for the right
one.  Replaces production functions while it runs; use it in a process of its
own."
  (let* ((rows
           (loop for (function mutant . targets) in *mutants*
                 collect (let ((report (call-with-mutant
                                        function mutant
                                        (lambda () (run-bundle :targets targets
                                                               :seeds seeds)))))
                           (list :function function
                                 :caught (loop for entry in (getf report :entries)
                                               collect (list (getf entry :name)
                                                             (getf entry :seed)
                                                             (not (eq :passed
                                                                      (getf entry :status)))))))))
         (problems (loop for row in rows
                         for missed = (remove-if #'third (getf row :caught))
                         when missed
                           collect (list :mutant-not-caught (getf row :function)
                                         :by (mapcar (lambda (miss) (subseq miss 0 2)) missed)))))
    (list :ok (null problems) :problems problems :mutants rows)))

;;; --- Reporting ---

(defun print-entry (entry stream)
  "Print one line for ENTRY, and its counterexample when it did not pass."
  (let ((problems (judge-entry entry)))
    (format stream "~&~:[PASS~;FAIL~] ~(~A~) ~(~A~) seed=~A status=~(~A~) trials=~A~@[ rejected=~A~]~%"
            problems (getf entry :kind) (getf entry :name) (getf entry :seed)
            (getf entry :status) (getf entry :trials) (getf entry :rejected))
    (when problems
      (let ((*print-length* 20) (*print-level* 6))
        (format stream "     problems: ~S~%" problems)
        (loop for key in '(:failure-phase :failure-reason :condition
                           :counterexample :shrunk-counterexample)
              for value = (getf entry key)
              when value
                do (format stream "     ~(~A~): ~S~%" key value))))))

(defun print-report (report &optional (stream *standard-output*))
  "Print REPORT, one line per run, then its verdict."
  (dolist (entry (getf report :entries))
    (print-entry entry stream))
  (dolist (row (getf report :mutants))
    (format stream "~&mutant ~S: ~{~A~^, ~}~%" (getf row :function)
            (mapcar (lambda (caught)
                      (format nil "~(~A~)@~A ~:[MISSED~;caught~]"
                              (first caught) (second caught) (third caught)))
                    (getf row :caught))))
  (format stream "~&~:[FAILED~;OK~]~@[: ~S~]~%" (getf report :ok) (getf report :problems)))

(defun main (&key (mode :check) report-pathname)
  "Run MODE (:CHECK or :NEGATIVE-CONTROL), print the report, write it to
REPORT-PATHNAME as one Lisp form when given, and return 0 when it is OK and 1
otherwise."
  (let ((report (ecase mode
                  (:check (run-bundle))
                  (:negative-control (run-negative-control)))))
    (print-report report)
    (when report-pathname
      (with-open-file (out report-pathname :direction :output :if-exists :supersede)
        (with-standard-io-syntax
          (let ((*print-readably* nil))
            (prin1 report out)
            (terpri out)))))
    (if (getf report :ok) 0 1)))
