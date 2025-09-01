;; Decentralized Database Services Contract
;; P2P database hosting with data integrity guarantees

;; Define constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u401))
(define-constant ERR_DATABASE_NOT_FOUND (err u404))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u402))
(define-constant ERR_INVALID_HASH (err u400))
(define-constant ERR_NODE_NOT_FOUND (err u405))
(define-constant ERR_ALREADY_EXISTS (err u409))

;; Define data variables
(define-data-var contract-owner principal CONTRACT_OWNER)
(define-data-var min-storage-fee uint u1000000) ;; 1 STX minimum
(define-data-var reputation-threshold uint u75) ;; 75% minimum reputation

;; Define data structures
(define-map databases
  { db-id: (string-ascii 64) }
  {
    owner: principal,
    name: (string-utf8 256),
    description: (string-utf8 512),
    data-hash: (buff 32),
    size: uint,
    replication-factor: uint,
    created-at: uint,
    updated-at: uint,
    is-public: bool,
    storage-fee: uint
  }
)

(define-map storage-nodes
  { node-id: principal }
  {
    reputation: uint,
    total-storage: uint,
    available-storage: uint,
    uptime-score: uint,
    last-ping: uint,
    rewards-earned: uint,
    is-active: bool
  }
)

(define-map data-replicas
  { db-id: (string-ascii 64), node-id: principal }
  {
    hash-verification: (buff 32),
    stored-at: uint,
    last-verified: uint,
    integrity-score: uint
  }
)

(define-map access-permissions
  { db-id: (string-ascii 64), user: principal }
  { can-read: bool, can-write: bool, granted-at: uint }
)

;; Storage node registration
(define-public (register-storage-node (storage-capacity uint))
  (let ((node-id tx-sender))
    (if (is-some (map-get? storage-nodes { node-id: node-id }))
      ERR_ALREADY_EXISTS
      (begin
        (map-set storage-nodes
          { node-id: node-id }
          {
            reputation: u100,
            total-storage: storage-capacity,
            available-storage: storage-capacity,
            uptime-score: u100,
            last-ping: block-height,
            rewards-earned: u0,
            is-active: true
          }
        )
        (ok true)
      )
    )
  )
)

;; Create a new database
(define-public (create-database 
    (db-id (string-ascii 64)) 
    (name (string-utf8 256))
    (description (string-utf8 512))
    (initial-data-hash (buff 32))
    (size uint)
    (replication-factor uint)
    (is-public bool)
  )
  (let ((storage-cost (* size replication-factor (var-get min-storage-fee))))
    (if (is-some (map-get? databases { db-id: db-id }))
      ERR_ALREADY_EXISTS
      (if (>= (stx-get-balance tx-sender) storage-cost)
        (begin
          ;; Transfer payment for storage
          (try! (stx-transfer? storage-cost tx-sender (as-contract tx-sender)))
          
          ;; Create database entry
          (map-set databases
            { db-id: db-id }
            {
              owner: tx-sender,
              name: name,
              description: description,
              data-hash: initial-data-hash,
              size: size,
              replication-factor: replication-factor,
              created-at: block-height,
              updated-at: block-height,
              is-public: is-public,
              storage-fee: storage-cost
            }
          )
          
          ;; Set owner permissions
          (map-set access-permissions
            { db-id: db-id, user: tx-sender }
            { can-read: true, can-write: true, granted-at: block-height }
          )
          
          (ok true)
        )
        ERR_INSUFFICIENT_PAYMENT
      )
    )
  )
)

;; Update database data with integrity verification
(define-public (update-database (db-id (string-ascii 64)) (new-data-hash (buff 32)))
  (let ((database (unwrap! (map-get? databases { db-id: db-id }) ERR_DATABASE_NOT_FOUND)))
    (if (or 
          (is-eq tx-sender (get owner database))
          (default-to false (get can-write (map-get? access-permissions { db-id: db-id, user: tx-sender })))
        )
      (begin
        (map-set databases
          { db-id: db-id }
          (merge database { 
            data-hash: new-data-hash,
            updated-at: block-height
          })
        )
        (ok true)
      )
      ERR_NOT_AUTHORIZED
    )
  )
)

;; Store data replica on a storage node
(define-public (store-replica 
    (db-id (string-ascii 64)) 
    (node-id principal) 
    (hash-verification (buff 32))
  )
  (let (
    (database (unwrap! (map-get? databases { db-id: db-id }) ERR_DATABASE_NOT_FOUND))
    (node (unwrap! (map-get? storage-nodes { node-id: node-id }) ERR_NODE_NOT_FOUND))
  )
    (if (and 
          (is-eq tx-sender (get owner database))
          (>= (get reputation node) (var-get reputation-threshold))
          (get is-active node)
        )
      (begin
        ;; Update node available storage
        (map-set storage-nodes
          { node-id: node-id }
          (merge node { 
            available-storage: (- (get available-storage node) (get size database))
          })
        )
        
        ;; Store replica information
        (map-set data-replicas
          { db-id: db-id, node-id: node-id }
          {
            hash-verification: hash-verification,
            stored-at: block-height,
            last-verified: block-height,
            integrity-score: u100
          }
        )
        
        (ok true)
      )
      ERR_NOT_AUTHORIZED
    )
  )
)

;; Verify data integrity
(define-public (verify-integrity (db-id (string-ascii 64)) (node-id principal) (provided-hash (buff 32)))
  (let (
    (database (unwrap! (map-get? databases { db-id: db-id }) ERR_DATABASE_NOT_FOUND))
    (replica (unwrap! (map-get? data-replicas { db-id: db-id, node-id: node-id }) ERR_NODE_NOT_FOUND))
    (node (unwrap! (map-get? storage-nodes { node-id: node-id }) ERR_NODE_NOT_FOUND))
  )
    (if (is-eq provided-hash (get data-hash database))
      (begin
        ;; Update replica verification
        (map-set data-replicas
          { db-id: db-id, node-id: node-id }
          (merge replica { 
            last-verified: block-height,
            integrity-score: u100
          })
        )
        
        ;; Reward node for maintaining data integrity
        (let ((reward (/ (get storage-fee database) (get replication-factor database))))
          (try! (as-contract (stx-transfer? reward tx-sender node-id)))
          (map-set storage-nodes
            { node-id: node-id }
            (merge node { 
              rewards-earned: (+ (get rewards-earned node) reward),
              reputation: (min u100 (+ (get reputation node) u1))
            })
          )
        )
        
        (ok true)
      )
      (begin
        ;; Penalize node for data corruption
        (map-set storage-nodes
          { node-id: node-id }
          (merge node { 
            reputation: (if (> (get reputation node) u10) (- (get reputation node) u10) u0)
          })
        )
        
        (map-set data-replicas
          { db-id: db-id, node-id: node-id }
          (merge replica { 
            last-verified: block-height,
            integrity-score: (if (> (get integrity-score replica) u10) (- (get integrity-score replica) u10) u0)
          })
        )
        
        ERR_INVALID_HASH
      )
    )
  )
)

;; Grant access permissions
(define-public (grant-access (db-id (string-ascii 64)) (user principal) (can-read bool) (can-write bool))
  (let ((database (unwrap! (map-get? databases { db-id: db-id }) ERR_DATABASE_NOT_FOUND)))
    (if (is-eq tx-sender (get owner database))
      (begin
        (map-set access-permissions
          { db-id: db-id, user: user }
          { can-read: can-read, can-write: can-write, granted-at: block-height }
        )
        (ok true)
      )
      ERR_NOT_AUTHORIZED
    )
  )
)

;; Node heartbeat to maintain uptime score
(define-public (node-heartbeat)
  (let (
    (node-id tx-sender)
    (node (unwrap! (map-get? storage-nodes { node-id: node-id }) ERR_NODE_NOT_FOUND))
  )
    (map-set storage-nodes
      { node-id: node-id }
      (merge node { 
        last-ping: block-height,
        uptime-score: (min u100 (+ (get uptime-score node) u1))
      })
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-database (db-id (string-ascii 64)))
  (map-get? databases { db-id: db-id })
)

(define-read-only (get-storage-node (node-id principal))
  (map-get? storage-nodes { node-id: node-id })
)

(define-read-only (get-replica-info (db-id (string-ascii 64)) (node-id principal))
  (map-get? data-replicas { db-id: db-id, node-id: node-id })
)

(define-read-only (check-access (db-id (string-ascii 64)) (user principal))
  (map-get? access-permissions { db-id: db-id, user: user })
)

(define-read-only (get-min-storage-fee)
  (var-get min-storage-fee)
)

;; Admin functions
(define-public (set-min-storage-fee (new-fee uint))
  (if (is-eq tx-sender (var-get contract-owner))
    (begin
      (var-set min-storage-fee new-fee)
      (ok true)
    )
    ERR_NOT_AUTHORIZED
  )
)

(define-public (set-reputation-threshold (new-threshold uint))
  (if (is-eq tx-sender (var-get contract-owner))
    (begin
      (var-set reputation-threshold new-threshold)
      (ok true)
    )
    ERR_NOT_AUTHORIZED
  )
)