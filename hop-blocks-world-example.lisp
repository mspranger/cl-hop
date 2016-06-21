
(in-package :cl-hop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BLOCKS WORLD EXAMPLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OPERATORS

(defun pickup (state b)
  (when (and (eq (assqv b (assqv 'pos state)) 'table)
             (assqv b (assqv 'clear state))
             (null (assqv 'holding state)))
    `((pos . ((,b . hand) ,@(remove b (assqv 'pos state) :key 'car)))
      (clear . ((,b . nil) ,@(remove b (assqv 'clear state) :key 'car)))
      (holding . ,b))))

(defun unstack (state b1 b2)
  (when (and (eq (assqv b1 (assqv 'pos state)) b2)
             (not (eq b2 'table))
             (assqv b1 (assqv 'clear state))
             (null (assqv 'holding state)))
    `((pos . ((,b1 . hand) ,@(remove b1 (assqv 'pos state) :key 'car)))
      (clear . ((,b1 . nil) (,b2 t) ,@(remove b2
                                              (remove b1 (assqv 'clear state)
                                                      :key 'car)
                                              :key 'car)))
      (holding . ,b1))))

(defun put-down (state b)
  (when (eq (assqv b (assqv 'pos state)) 'hand)
    `((pos . ((,b . table) ,@(remove b (assqv 'pos state) :key 'car)))
      (clear . ((,b t) ,@(remove b (assqv 'clear state) :key 'car)))
      (holding . nil))))

(defun stack (state b c)
  (when (and (eq (assqv b (assqv 'pos state)) 'hand)
             (assqv c (assqv 'clear state)))
    `((pos . ((,b . ,c) ,@(remove b (assqv 'pos state) :key 'car)))
      (clear . ((,b . t) (,c . nil)
                ,@(remove c (remove b (assqv 'clear state) :key 'car) :key 'car)))
      (holding . nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; METHODS

(defun m-get (state b1)
  (when (assqv b1 (assqv 'clear state))
    (if (eq 'table (assqv b1 (assqv 'pos state) ))
      `((pickup ,b1))
      `((unstack ,b1 ,(assqv b1 (assqv 'pos state)))))))

(defun m-put (state b1 b2)
  (when (eq b1 (assqv 'holding state))
    (if (eq b2 'table)
      `((put-down ,b1))
      `((stack ,b1 ,b2)))))
  
(defun m-move-1 (state b1 dest)
  (declare (ignore state))
  `((m-get ,b1)
    (m-put ,b1 ,dest)))

(defun helper-is-done (b1 state goal)
  (cond
   ((eq b1 'table) t)
   ((and (member b1 (assqv 'pos goal) :key 'first)
         (not (eq (assqv b1 (assqv 'pos goal))
                  (assqv b1 (assqv 'pos state)))))
    nil)
   ((eq 'table (assqv b1 (assqv 'pos state))) t)
   (t (helper-is-done (assqv b1 (assqv 'pos state)) state goal))))
  
(defun m-move-blocks (state goal)
  """
    This method implements the following block-stacking algorithm:
    If there's a block that can be moved to its final position, then
    do so and call move_blocks recursively. Otherwise, if there's a
    block that needs to be moved and can be moved to the table, then 
    do so and call move_blocks recursively. Otherwise, no blocks need
    to be moved.
    """
  (let ((blocks-left (loop for b1 in (mapcar 'car (assqv 'clear state))
                           
                           unless (helper-is-done b1 state goal)
                           collect b1)))
    ;; if nothing is left then success
    (when (null blocks-left) (return-from m-move-blocks t))
    ;; check if we can move blocks to final position
    (loop for b1 in blocks-left
          ;; move to table
          if (and (assqv b1 (assqv 'clear state))
                  (or (not (member b1 (assqv 'pos goal) :key 'first))
                      (eq 'table (assqv b1 (assqv 'pos goal)))))
          do (return-from m-move-blocks `((m-move-1 ,b1 table)
                                          (m-move-blocks ,goal)))
          ;; move to block
          else if (and (assqv b1 (assqv 'clear state))
                       (helper-is-done (assqv b1 (assqv 'pos goal)) state goal)
                       (assqv (assqv b1 (assqv 'pos goal))
                              (assqv 'clear state)))
          do (return-from m-move-blocks `((m-move-1 ,b1 ,(assqv b1 (assqv 'pos goal)))
                                          (m-move-blocks ,goal))))
    ;; check if any block is waiting
          
    ;; any blocks left unprocessed
    (loop for b1 in blocks-left
          when (and (assqv b1 (assqv 'clear state))
                    (not (eq 'table (assqv b1 (assqv 'pos state)))))
          do (return-from m-move-blocks
               `((m-move-1 ,b1 table)
                 (m-move-blocks ,goal)))))
  ;; we failed
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXAMPLE

(let ((state-1 `((pos . ((a . b)(b . table)(c . table)))
                 (clear . ((a . t)(b . nil)(c . t)))
                 (holding . nil))))
  ;; fails
  (assert (null (hop state-1 `((pickup a))
                     :operators '(pickup unstack put-down stack)
                     :methods '((m-move-blocks . (m-move-blocks))
                                (m-move-1 . (m-move-1))
                                (m-put . (m-put))
                                (m-get . (m-get))))))
  (assert (null (hop state-1 `((pickup b))
                     :operators '(pickup unstack put-down stack)
                     :methods '((m-move-blocks . (m-move-blocks))
                                (m-move-1 . (m-move-1))
                                (m-put . (m-put))
                                (m-get . (m-get))))))
  ;; succeeded
  (assert (hop state-1 `((pickup c))
               :operators '(pickup unstack put-down stack)
               :methods '((m-move-blocks . (m-move-blocks))
                          (m-move-1 . (m-move-1))
                          (m-put . (m-put))
                          (m-get . (m-get)))))
  (assert (hop state-1 `((unstack a b))
               :operators '(pickup unstack put-down stack)
               :methods '((m-move-blocks . (m-move-blocks))
                          (m-move-1 . (m-move-1))
                          (m-put . (m-put))
                          (m-get . (m-get)))))
  (assert (hop state-1 `((m-get a))
               :operators '(pickup unstack put-down stack)
               :methods '((m-move-blocks . (m-move-blocks))
                          (m-move-1 . (m-move-1))
                          (m-put . (m-put))
                          (m-get . (m-get)))))
  ;; fail
  (assert (null (hop state-1 `((m-get b))
                     :operators '(pickup unstack put-down stack)
                     :methods '((m-move-blocks . (m-move-blocks))
                                (m-move-1 . (m-move-1))
                                (m-put . (m-put))
                                (m-get . (m-get))))))
  (assert (hop state-1 `((m-get c))
               :operators '(pickup unstack put-down stack)
               :methods '((m-move-blocks . (m-move-blocks))
                          (m-move-1 . (m-move-1))
                          (m-put . (m-put))
                          (m-get . (m-get))))))

(let ((state-1 '((pos . ((a . b)(b . table)(c . table)))
                 (clear . ((a . t)(b . nil)(c . t)))
                 (holding . nil)))
      (goal-1-a '((pos . ((c . b)(b . a)(a . table)))
                  (clear . ((c . t)(b . nil)(a . nil)))
                  (holding . nil)))
      (goal-1-b '((pos . ((c . b)(b . a))))))
  (assert (hop state-1 `((m-move-blocks ,goal-1-a)) :operators '(pickup unstack put-down stack)
               :methods '((m-move-blocks . (m-move-blocks))
                          (m-move-1 . (m-move-1))
                          (m-put . (m-put))
                          (m-get . (m-get)))))
  
  (assert (hop state-1 `((m-move-blocks ,goal-1-b)) :operators '(pickup unstack put-down stack)
               :methods '((m-move-blocks . (m-move-blocks))
                          (m-move-1 . (m-move-1))
                          (m-put . (m-put))
                          (m-get . (m-get))))))

;; one planning problem (different formulations - b is described less complete than a)
(let ((state-1 '((pos . ((a . b)(b . table)(c . table)))
                 (clear . ((a . t)(b . nil)(c . t)))
                 (holding . nil)))
      (goal-1-a '((pos . ((c . b)(b . a)(a . table)))
                  (clear . ((c . t)(b . nil)(a . nil)))
                  (holding . nil)))
      (goal-1-b '((pos . ((c . b)(b . a))))))
  (assert (hop state-1 `((m-move-blocks ,goal-1-a)) :operators '(pickup unstack put-down stack)
               :methods '((m-move-blocks . (m-move-blocks))
                          (m-move-1 . (m-move-1))
                          (m-put . (m-put))
                          (m-get . (m-get)))))
  
  (assert (hop state-1 `((m-move-blocks ,goal-1-b)) :operators '(pickup unstack put-down stack)
               :methods '((m-move-blocks . (m-move-blocks))
                          (m-move-1 . (m-move-1))
                          (m-put . (m-put))
                          (m-get . (m-get))))))

;; two planning problems
(let ((state-2 '((pos . ((a . c)(b . d)(c . table)(d . table)))
                 (clear . ((a . t)(b . t)(c . nil)(d . nil)))
                 (holding . nil)))
      (goal-2-a '((pos . ((b . c)(a . d)(c . table)(d . table)))
                  (clear . ((a . t)(b . t)(c . nil)(d . nil)))
                  (holding . nil)))
      (goal-2-b '((pos . ((b . c)(a . d))))))
  (assert (hop state-2 `((m-move-blocks ,goal-2-a)) :operators '(pickup unstack put-down stack)
               :methods '((m-move-blocks . (m-move-blocks))
                          (m-move-1 . (m-move-1))
                          (m-put . (m-put))
                          (m-get . (m-get)))))
  (assert (hop state-2 `((m-move-blocks ,goal-2-b)) :operators '(pickup unstack put-down stack)
               :methods '((m-move-blocks . (m-move-blocks))
                          (m-move-1 . (m-move-1))
                          (m-put . (m-put))
                          (m-get . (m-get))))))

;; bw_large_d from SHOP distribution
(let ((state-3 `((pos . ((1 . 12)(12 . 13)(13 . table)(11 . 10)(10 . 5)(5 . 4)(4 . 14)
                         (14 . 15)(15 . table)(9 . 8)(8 . 7)
                         (7 . 6)(6 . table)(19 . 18)(18 . 17)(17 . 16)(16 . 3)(3 . 2)(2 . table)))
                 (clear . ,(loop for i from 1 to 19
                                 if (member i '(1 9 11 19))
                                 collect (cons i t)
                                 else collect (cons i nil)))
                 (holding . nil)))
      (goal-3 `((pos . ((15 . 13)
                        (13 . 8)
                        (8 . 9)
                        (9 . 4)
                        (4 . table)
                        (12 . 2)
                        (2 . 3)
                        (3 . 16)
                        (16 . 11)
                        (11 . 7)
                        (7 . 6)
                        (6 . table)
                        
                        ))
                (clear . ,(loop for i in '(12 15 17)
                                collect (cons i t)))
                (holding . nil))))
  (assert (hop state-3 `((m-move-blocks ,goal-3)) :operators '(pickup unstack put-down stack)
               :methods '((m-move-blocks . (m-move-blocks))
                          (m-move-1 . (m-move-1))
                          (m-put . (m-put))
                          (m-get . (m-get))))))
 
