(in-package #:colander/finite-automata)

(defclass production ()
  ((cli-spec :initarg :cli-spec)))
(defclass item ()
  ((prod-id :initarg :prod-id)
   (dot :initarg :dot)))

(defmethod similar-p ((obj1 item) (obj2 item))
  (and (= (slot-value obj1 'prod-id) (slot-value obj2 'prod-id))
       (= (slot-value obj1 'dot) (slot-value obj2 'dot))))

(defun item-advance (item)
  (with-slots (dot prod-id) item
    (make-instance 'item :prod-id prod-id :dot (1+ dot))))
(defun item-prod (item prods)
  (aref prods (slot-value item 'prod-id)))
(defun item-at-dot (item prods)
  (with-slots (cli-spec) (item-prod item prods)
    (nth (slot-value item 'dot) cli-spec)))
