
(defpackage :cl-hop
  (:documentation "A package for hop HTN planner after py-hop")
  (:use :common-lisp))

(in-package :cl-hop)

(defun assqv (obj list &key (test #'eq))
  "Calls assoc with eq as test predicate, and returns cdr of found value or nil."
  (declare (type list list))
  (cdr (assoc obj list :test test)))

(defun hop (state tasks &key silent operators methods)
  """ State - an association list of state variables `((state-property-1 . t))
      task list of things that should happen - a subset of possible states
      operators - list of symbols (defun/defmethod) - return new-state or nil
      methods - association list for (method-name . (method-1 method-2 method-3))
                methods return nil - FAIL, t - success, (task-1 task-2) - list of new subtasks"""
  (unless silent
    (format t "~%hop started :state ~a :tasks ~a :operators ~a :methods ~a"
            state tasks operators methods))
  (let ((result (hop-plan state tasks '() 0 :operators operators :methods methods :silent silent)))
    (unless silent
      (format t "~%hop finished :state ~a :tasks ~a :operators ~a :methods ~a :result ~a"
              state tasks operators methods result))
    (reverse result)))

(defun hop-plan (state tasks plan depth &key silent operators methods)
  (unless silent
    (format t "~%~ahop-plan~%~a:tasks ~a~%~a:plan ~a~%~a:depth ~a~%~a:state ~a"
            (make-string depth :initial-element #\space)
            (make-string depth :initial-element #\space)
            tasks
            (make-string depth :initial-element #\space)
            plan
            (make-string depth :initial-element #\space)
            depth
            (make-string depth :initial-element #\space)
            state))
  ;; no tasks left -> finished
  (when (null tasks)
    (unless silent
      (format t "~%~ahop-plan no tasks left :returns ~a"
              (make-string depth :initial-element #\space) plan))
    (return-from hop-plan plan))
  ;; check operator and methods
  (let* ((task1 (first tasks))
         (operator (find (first task1) operators))
         (possible-methods (assqv (first task1) methods)))
    (unless silent
      (format t "~%~ahop-plan :task ~a"
              (make-string depth :initial-element #\space) task1))
    (assert (and (or operator possible-methods)
                 (not (and operator possible-methods))))
 
    (cond
     ;; handle operator 
     (operator
      ;; perform action/compute new state
      (unless silent
        (format t "~%~ahop-plan apply :action ~a"
                (make-string depth :initial-element #\space) task1))
      ;; apply operator
      (let ((new-state (apply operator state (cdr task1))))
        (if new-state
          (progn
            ;; if new-state, go ahead and deal with remaining tasks
            (unless silent (format t "~%~ahop-plan action applied :new-state ~a" (make-string depth :initial-element #\space)
                                   new-state))
            (let ((solution (hop-plan new-state (cdr tasks) (push task1 plan) (+ 1 depth)
                                      :silent silent
                                      :operators operators
                                      :methods methods)))
              (when solution
                (unless silent
                  (format t "~%~ahop-plan :returns ~a"
                          (make-string depth :initial-element #\space) solution))
                (return-from hop-plan solution))))
          (progn
            (unless silent (format t "~%~ahop-plan FAILURE action applied no new-state"
                                   (make-string depth :initial-element #\space)))
            (return-from hop-plan nil)))))
     ;; handle method
     (possible-methods
      ;; get possible methods, try each of them
      (unless silent
        (format t "~%~ahop-plan :method ~a :possible-methods ~a" (make-string depth :initial-element #\space)
                (first task1)
                possible-methods))
      (loop
       for possible-method in possible-methods
       for subtasks = (apply possible-method state (cdr task1)) ;; this returns a list of operators and or methods
       if (eq subtasks t)
       do (unless silent
            (format t "~%~ahop-plan no tasks left :returns ~a"
                    (make-string depth :initial-element #\space) plan))
       and do (return-from hop-plan plan)
       else if subtasks
       do (unless silent
            (format t "~%~ahop-plan :possible-method ~a :subtasks ~a"
                    (make-string depth :initial-element #\space) possible-method subtasks))
       and do (let ((solution (hop-plan state (append subtasks (cdr tasks)) plan (+ 1 depth)
                                        :silent silent
                                        :operators operators
                                        :methods methods)))
                (if solution
                  (progn (unless silent
                           (format t "~%~ahop-plan :returns ~a"
                                   (make-string depth :initial-element #\space) solution))
                    (return-from hop-plan solution)))))
      (unless silent
        (format t "~%~ahop-plan FAILURE no method found that works"
                (make-string depth :initial-element #\space))))))
  nil)
