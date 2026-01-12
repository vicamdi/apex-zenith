;; Apex Zenith - Decentralized Storage Platform
;; A smart contract for managing decentralized storage with PoSQ consensus

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-params (err u105))

;; Minimum stake required for storage nodes (in microSTX)
(define-constant min-stake u1000000)

;; Data Variables
(define-data-var base-storage-price uint u100)
(define-data-var total-storage-nodes uint u0)
(define-data-var total-storage-requests uint u0)

;; Storage Node Registry
(define-map storage-nodes
    principal
    {
        stake: uint,
        uptime-score: uint,
        retrieval-speed: uint,
        data-integrity: uint,
        total-stored: uint,
        active: bool,
        registered-at: uint
    }
)

;; Storage Requests
(define-map storage-requests
    uint
    {
        owner: principal,
        data-hash: (string-ascii 64),
        size: uint,
        price: uint,
        storage-node: (optional principal),
        created-at: uint,
        expires-at: uint,
        active: bool
    }
)

;; Storage Futures - lock in prices
(define-map storage-futures
    {owner: principal, future-id: uint}
    {
        locked-price: uint,
        duration: uint,
        storage-amount: uint,
        created-at: uint,
        active: bool
    }
)

;; Node Performance Tracking
(define-map node-performance
    {node: principal, period: uint}
    {
        successful-retrievals: uint,
        failed-retrievals: uint,
        total-uptime: uint,
        quality-score: uint
    }
)

;; Read-only functions

(define-read-only (get-storage-node (node principal))
    (map-get? storage-nodes node)
)

(define-read-only (get-storage-request (request-id uint))
    (map-get? storage-requests request-id)
)

(define-read-only (get-base-price)
    (ok (var-get base-storage-price))
)

(define-read-only (get-total-nodes)
    (ok (var-get total-storage-nodes))
)

(define-read-only (calculate-storage-price (size uint))
    (let
        (
            (base-price (var-get base-storage-price))
            (total-price (* size base-price))
        )
        (ok total-price)
    )
)

(define-read-only (get-node-quality-score (node principal))
    (let
        (
            (node-data (unwrap! (map-get? storage-nodes node) (err err-not-found)))
        )
        (ok (+ 
            (/ (get uptime-score node-data) u3)
            (/ (get retrieval-speed node-data) u3)
            (/ (get data-integrity node-data) u3)
        ))
    )
)

;; Public functions

;; Register as a storage node
(define-public (register-storage-node (stake-amount uint))
    (let
        (
            (node tx-sender)
            (existing-node (map-get? storage-nodes node))
        )
        (asserts! (is-none existing-node) err-already-exists)
        (asserts! (>= stake-amount min-stake) err-insufficient-funds)
        
        ;; Store stake (simplified - in production would use STX transfer)
        (map-set storage-nodes node
            {
                stake: stake-amount,
                uptime-score: u100,
                retrieval-speed: u100,
                data-integrity: u100,
                total-stored: u0,
                active: true,
                registered-at: block-height
            }
        )
        
        (var-set total-storage-nodes (+ (var-get total-storage-nodes) u1))
        (ok true)
    )
)

;; Create a storage request
(define-public (create-storage-request (data-hash (string-ascii 64)) (size uint) (duration uint))
    (let
        (
            (request-id (+ (var-get total-storage-requests) u1))
            (price (unwrap! (calculate-storage-price size) err-invalid-params))
        )
        (asserts! (> size u0) err-invalid-params)
        (asserts! (> duration u0) err-invalid-params)
        
        (map-set storage-requests request-id
            {
                owner: tx-sender,
                data-hash: data-hash,
                size: size,
                price: price,
                storage-node: none,
                created-at: block-height,
                expires-at: (+ block-height duration),
                active: true
            }
        )
        
        (var-set total-storage-requests request-id)
        (ok request-id)
    )
)

;; Storage node accepts a storage request
(define-public (accept-storage-request (request-id uint))
    (let
        (
            (request (unwrap! (map-get? storage-requests request-id) err-not-found))
            (node-data (unwrap! (map-get? storage-nodes tx-sender) err-unauthorized))
        )
        (asserts! (get active node-data) err-unauthorized)
        (asserts! (get active request) err-invalid-params)
        (asserts! (is-none (get storage-node request)) err-already-exists)
        
        ;; Assign storage node to request
        (map-set storage-requests request-id
            (merge request {storage-node: (some tx-sender)})
        )
        
        ;; Update node's total stored data
        (map-set storage-nodes tx-sender
            (merge node-data {
                total-stored: (+ (get total-stored node-data) (get size request))
            })
        )
        
        (ok true)
    )
)

;; Update node performance metrics (PoSQ)
(define-public (update-node-performance 
    (uptime-score uint) 
    (retrieval-speed uint) 
    (data-integrity uint))
    (let
        (
            (node-data (unwrap! (map-get? storage-nodes tx-sender) err-not-found))
        )
        (asserts! (get active node-data) err-unauthorized)
        (asserts! (<= uptime-score u100) err-invalid-params)
        (asserts! (<= retrieval-speed u100) err-invalid-params)
        (asserts! (<= data-integrity u100) err-invalid-params)
        
        (map-set storage-nodes tx-sender
            (merge node-data {
                uptime-score: uptime-score,
                retrieval-speed: retrieval-speed,
                data-integrity: data-integrity
            })
        )
        
        (ok true)
    )
)

;; Create a storage future (lock in price)
(define-public (create-storage-future 
    (future-id uint)
    (storage-amount uint) 
    (duration uint))
    (let
        (
            (current-price (var-get base-storage-price))
        )
        (asserts! (> storage-amount u0) err-invalid-params)
        (asserts! (> duration u0) err-invalid-params)
        
        (map-set storage-futures {owner: tx-sender, future-id: future-id}
            {
                locked-price: current-price,
                duration: duration,
                storage-amount: storage-amount,
                created-at: block-height,
                active: true
            }
        )
        
        (ok true)
    )
)

;; Admin function to update base storage price
(define-public (update-base-price (new-price uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set base-storage-price new-price)
        (ok true)
    )
)

;; Deactivate a storage node
(define-public (deactivate-node)
    (let
        (
            (node-data (unwrap! (map-get? storage-nodes tx-sender) err-not-found))
        )
        (map-set storage-nodes tx-sender
            (merge node-data {active: false})
        )
        (ok true)
    )
)