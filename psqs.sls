#!r6rs
;;; psqs.sls --- Priority Search Queues

;; Copyright (C) 2012 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;;; Documentation
;;
;; Priority search queues are a combination of two common abstract
;; data types: finite maps, and priority queues. As such, it provides
;; for access, insertion, removal and update on arbitrary keys, as
;; well as for easy removal of the element with the lowest priority.
;;
;; Note: where a procedure takes a key or priority these are expected
;; to be compatible with the relevant ordering procedures on the psq.
;;
;;;; Basic operations
;;
;; make-psq : < < -> psq
;; takes a two ordering procedures, one for keys, and another for
;; priorities, and returns an empty priority search queue
;;
;; psq? : obj -> boolean
;; returns #t if the object is a priority search queue, #f otherwise.
;;
;; psq-empty? : psq -> boolean
;; returns #t if the priority search queue contains no elements, #f
;; otherwise.
;;
;;;; Finite map operations
;;
;; psq-ref : psq key -> priority
;; returns the priority of a key if it is in the priority search
;; queue. If the key is not in the priority queue an
;; assertion-violation is raised.
;;
;; psq-set : psq key priority -> psq
;; returns the priority search queue obtained from inserting a key
;; with a given priority. If the key is already in the priority search
;; queue, it updates the priority to the new value.
;;
;; psq-update : psq key (priority -> priority) priority -> psq
;; returns the priority search queue obtained by modifying the
;; priority of key, by the given function. If the key is not in the
;; priority search queue, it is inserted with the priority obtained by
;; calling the function on the default value.
;;
;; psq-delete : psq key -> psq
;; returns the priority search queue obtained by removing the
;; key-priority association from the priority search queue. If the key
;; is not in the queue, then the returned search queue will be the
;; same as the original.
;;
;; psq-contains? : psq key -> boolean
;; returns #t if there is an association for the given key in the
;; priority search queue, #f otherwise.
;;
;;;; Priority queue operations
;;
;; psq-min : psq -> key
;;
;; returns the key of the minimum association in the priority search
;; queue. If the queue is empty, an assertion violation is raised.
;;
;; psq-delete-min : psq -> psq
;; returns the priority search queue obtained by removing the minimum
;; association in the priority search queue. If the queue is empty, an
;; assertion violation is raised.
;;
;; psq-pop : psq -> key + psq
;; returns two values: the minimum key and the priority search queue
;; obtained by removing the minimum association from the original
;; queue. If the queue is empty, an assertion violation is raised.
;;
(library (pfds psqs)
(export make-psq
        psq?
        psq-empty?
        ;; map operations
        psq-ref
        psq-set
        psq-update
        psq-delete
        psq-contains?
        ;; priority queue operations
        psq-min
        psq-delete-min
        psq-pop
        )
(import (except (rnrs) min))

;;; record types

(define-record-type void)

(define-record-type winner
  (fields key priority loser-tree maximum-key))

(define-record-type start)

(define-record-type loser
  (fields key priority left split-key right))

;;; functions
(define (maximum-key psq)
  (winner-maximum-key psq))

(define max-key maximum-key)

(define empty (make-void))

(define (singleton key priority)
  (make-winner key priority (make-start) key))

(define (play-match psq1 psq2 prio<?)
  (cond ((void? psq1) psq2)
        ((void? psq2) psq1)
        ((not (prio<? (winner-priority psq2)
                      (winner-priority psq1)))
         (let ((k1 (winner-key psq1))
               (p1 (winner-priority psq1))
               (t1 (winner-loser-tree psq1))
               (m1 (winner-maximum-key psq1))
               (k2 (winner-key psq2))
               (p2 (winner-priority psq2))
               (t2 (winner-loser-tree psq2))
               (m2 (winner-maximum-key psq2)))
           (make-winner k1
                        p1
                        (make-loser k2 p2 t1 m1 t2)
                        m2)))
        (else
         (let ((k1 (winner-key psq1))
               (p1 (winner-priority psq1))
               (t1 (winner-loser-tree psq1))
               (m1 (winner-maximum-key psq1))
               (k2 (winner-key psq2))
               (p2 (winner-priority psq2))
               (t2 (winner-loser-tree psq2))
               (m2 (winner-maximum-key psq2)))
           (make-winner k2
                        p2
                        (make-loser k1 p1 t1 m1 t2)
                        m2)))))

(define (second-best ltree key key<? prio<?)
  (if (start? ltree)
      (make-void)
      (let ((k (loser-key ltree))
            (p (loser-priority ltree))
            (l (loser-left ltree))
            (m (loser-split-key ltree))
            (r (loser-right ltree)))
        (if (not (key<? m k))
            (play-match (make-winner k p l m)
                        (second-best r key key<? prio<?)
                        prio<?)
            (play-match (second-best l m key<? prio<?)
                        (make-winner k p r key)
                        prio<?)))))

(define (delete-min psq key<? prio<?)
  ;; maybe void psqs should return void?
  (second-best (winner-loser-tree psq) (winner-maximum-key psq) key<? prio<?))

(define (psq-case psq empty-k singleton-k match-k key<?)
  (if (void? psq)
      (empty-k)
      (let ((k1 (winner-key psq))
            (p1 (winner-priority psq))
            (t  (winner-loser-tree psq))
            (m  (winner-maximum-key psq)))
        (if (start? t)
            (singleton-k k1 p1)
            (let ((k2 (loser-key t))
                  (p2 (loser-priority t))
                  (l  (loser-left t))
                  (s  (loser-split-key t))
                  (r  (loser-right t)))
              (if (not (key<? s k2))
                  (match-k (make-winner k2 p2 l s)
                           (make-winner k1 p1 r m))
                  (match-k (make-winner k1 p1 l s)
                           (make-winner k2 p2 r m))))))))

(define (lookup psq key default key<?)
  (psq-case psq
            (lambda () default)
            (lambda (k p)
              (if (or (key<? k key) (key<? key k))
                  default
                  p))
            (lambda (w1 w2)
              (if (not (key<? (max-key w1) key))
                  (lookup w1 key default key<?)
                  (lookup w2 key default key<?)))
            key<?))

(define (update psq key f default key<? prio<?)
  (psq-case psq
            (lambda () (singleton key (f default)))
            (lambda (k p)
              (cond ((key<? key k)
                     (play-match (singleton key (f default))
                                 (singleton k p)
                                 prio<?))
                    ((key<? k key)
                     (play-match (singleton k p)
                                 (singleton key (f default))
                                 prio<?))
                    (else
                     (singleton key (f p)))))
            (lambda (w1 w2)
              (if (not (key<? (max-key w1) key))
                  (play-match (update w1 key f default key<? prio<?)
                              w2
                              prio<?)
                  (play-match w1
                              (update w2 key f default key<? prio<?)
                              prio<?)))
            key<?))

(define (insert psq key val key<? prio<?)
  (psq-case psq
            (lambda () (singleton key val))
            (lambda (k p)
              (cond ((key<? key k)
                     (play-match (singleton key val)
                                 (singleton k p)
                                 prio<?))
                    ((key<? k key)
                     (play-match (singleton k p)
                                 (singleton key val)
                                 prio<?))
                    (else
                     (singleton key val))))
            (lambda (w1 w2)
              (if (not (key<? (max-key w1) key))
                  (play-match (insert w1 key val key<? prio<?) w2 prio<?)
                  (play-match w1 (insert w2 key val key<? prio<?) prio<?)))
            key<?))

(define (delete psq key key<? prio<?)
  (psq-case psq
            (lambda () empty)
            (lambda (k p)
              (if (or (key<? k key)
                      (key<? key k))
                  (singleton k p)
                  empty))
            (lambda (w1 w2)
              (if (not (key<? (max-key w1) key))
                  (play-match (delete w1 key key<? prio<?) w2 prio<?)
                  (play-match w1 (delete w2 key key<? prio<?) prio<?)))
            key<?))

(define (min tree)
  (when (void? tree)
    (assertion-violation 'psq-min
                         "Can't take the minimum of an empty priority search queue"))
  (winner-key tree))

(define (pop tree key<? prio<?)
  (when (void? tree)
    (assertion-violation 'psq-pop
                         "Can't pop from an empty priority search queue"))
  (values (winner-key tree)
          (delete-min tree key<? prio<?)))

;;; Exported Type

(define-record-type (psq %make-psq psq?)
  (fields key<? priority<? tree))

(define (%update-psq psq new-tree)
  (%make-psq (psq-key<? psq)
             (psq-priority<? psq)
             new-tree))

;;; Exported Procedures

(define (make-psq key<? priority<?)
  (%make-psq key<? priority<? (make-void)))

(define (psq-empty? psq)
  (assert (psq? psq))
  (void? (psq-tree psq)))

(define (psq-ref psq key)
  (define cookie (cons #f #f))
  (assert (psq? psq))
  (let ((val (lookup (psq-tree psq) key cookie (psq-key<? psq))))
    (if (eq? val cookie)
        (assertion-violation 'psq-ref "not in tree")
        val)))

(define (psq-set psq key priority)
  (assert (psq? psq))
  (%update-psq psq
               (insert (psq-tree psq) key priority (psq-key<? psq) (psq-priority<? psq))))

(define (psq-update psq key f default)
  (assert (psq? psq))
  (%update-psq psq (update (psq-tree psq) key f default (psq-key<? psq) (psq-priority<? psq))))

(define (psq-delete psq key)
  (assert (psq? psq))
  (%update-psq psq (delete (psq-tree psq) key (psq-key<? psq) (psq-priority<? psq))))

(define (psq-contains? psq key)
  (define cookie (cons #f #f))
  (assert (psq? psq))
  (let ((val (lookup (psq-tree psq) key cookie (psq-key<? psq))))
    (not (eq? val cookie))))

(define (psq-min psq)
  (assert (psq? psq))
  (min (psq-tree psq)))

(define (psq-delete-min psq)
  (assert (psq? psq))
  (%update-psq psq (delete-min (psq-tree psq) (psq-key<? psq) (psq-priority<? psq))))

(define (psq-pop psq)
  (assert (psq? psq))
  (let-values (((min rest) (pop (psq-tree psq) (psq-key<? psq) (psq-priority<? psq))))
    (values min (%update-psq psq rest))))

)