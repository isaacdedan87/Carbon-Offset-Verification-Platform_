;; Carbon Impact Reporting & Third-Party Auditing System

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u201))
(define-constant ERR-AUDITOR-NOT-CERTIFIED (err u202))
(define-constant ERR-AUDIT-NOT-FOUND (err u203))
(define-constant ERR-INVALID-SCORE (err u204))
(define-constant ERR-REPORT-NOT-FOUND (err u205))
(define-constant ERR-INVALID-STATUS (err u206))
(define-constant ERR-AUDIT-ALREADY-EXISTS (err u207))
(define-constant ERR-INSUFFICIENT-QUALIFICATION (err u208))

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Data variables for system management
(define-data-var audit-nonce uint u0)
(define-data-var report-nonce uint u0)
(define-data-var auditor-nonce uint u0)
(define-data-var min-audit-score uint u70)
(define-data-var audit-fee uint u1000)

;; Auditor certification levels
(define-constant CERT-BASIC u1)
(define-constant CERT-ADVANCED u2)
(define-constant CERT-EXPERT u3)

;; Audit status constants
(define-constant STATUS-PENDING u0)
(define-constant STATUS-IN-PROGRESS u1)
(define-constant STATUS-COMPLETED u2)
(define-constant STATUS-DISPUTED u3)

;; Impact categories for reporting
(define-constant IMPACT-CO2-REDUCTION u1)
(define-constant IMPACT-BIODIVERSITY u2)
(define-constant IMPACT-WATER-QUALITY u3)
(define-constant IMPACT-SOIL-HEALTH u4)
(define-constant IMPACT-COMMUNITY-BENEFIT u5)

;; Certified auditors registry
(define-map certified-auditors principal {
    certification-level: uint,
    specializations: (list 5 uint),
    audits-completed: uint,
    average-score: uint,
    certification-date: uint,
    active: bool
})

;; Project audit records
(define-map project-audits uint {
    project-id: uint,
    auditor: principal,
    audit-type: uint,
    compliance-score: uint,
    environmental-score: uint,
    social-score: uint,
    overall-score: uint,
    audit-date: uint,
    status: uint,
    findings: (string-utf8 500),
    recommendations: (string-utf8 500)
})

;; Impact reporting data
(define-map impact-reports uint {
    project-id: uint,
    reporting-period: uint,
    co2-reduced: uint,
    biodiversity-index: uint,
    water-quality-score: uint,
    soil-health-score: uint,
    community-impact-score: uint,
    report-date: uint,
    verified: bool,
    auditor: principal
})

;; Compliance tracking
(define-map compliance-history uint {
    project-id: uint,
    compliance-date: uint,
    regulation-type: (string-utf8 100),
    compliance-status: bool,
    violations: (list 5 (string-utf8 100)),
    corrective-actions: (list 5 (string-utf8 100))
})

;; Audit dispute resolution
(define-map audit-disputes uint {
    audit-id: uint,
    disputer: principal,
    dispute-reason: (string-utf8 300),
    dispute-date: uint,
    resolution: (string-utf8 300),
    resolved: bool,
    resolver: (optional principal)
})

;; Risk assessment scores
(define-map risk-assessments uint {
    project-id: uint,
    financial-risk: uint,
    environmental-risk: uint,
    operational-risk: uint,
    regulatory-risk: uint,
    overall-risk-score: uint,
    assessment-date: uint
})

;; Public function: Register as certified auditor
(define-public (register-auditor 
    (certification-level uint) 
    (specializations (list 5 uint)))
    (let ((auditor-id (var-get auditor-nonce)))
        (if (and 
            (>= certification-level CERT-BASIC)
            (<= certification-level CERT-EXPERT))
            (begin
                (map-set certified-auditors tx-sender {
                    certification-level: certification-level,
                    specializations: specializations,
                    audits-completed: u0,
                    average-score: u0,
                    certification-date: stacks-block-height,
                    active: true
                })
                (var-set auditor-nonce (+ auditor-id u1))
                (ok auditor-id))
            ERR-INVALID-SCORE)))

;; Public function: Conduct project audit
(define-public (conduct-audit 
    (project-id uint)
    (audit-type uint)
    (compliance-score uint)
    (environmental-score uint)
    (social-score uint)
    (findings (string-utf8 500))
    (recommendations (string-utf8 500)))
    (let 
        ((audit-id (var-get audit-nonce))
         (auditor-data (unwrap! (map-get? certified-auditors tx-sender) ERR-AUDITOR-NOT-CERTIFIED))
         (overall-score (calculate-overall-score compliance-score environmental-score social-score)))
        (if (and 
            (get active auditor-data)
            (<= compliance-score u100)
            (<= environmental-score u100)
            (<= social-score u100))
            (begin
                (map-set project-audits audit-id {
                    project-id: project-id,
                    auditor: tx-sender,
                    audit-type: audit-type,
                    compliance-score: compliance-score,
                    environmental-score: environmental-score,
                    social-score: social-score,
                    overall-score: overall-score,
                    audit-date: stacks-block-height,
                    status: STATUS-COMPLETED,
                    findings: findings,
                    recommendations: recommendations
                })
                (update-auditor-stats tx-sender overall-score)
                (var-set audit-nonce (+ audit-id u1))
                (ok audit-id))
            ERR-INVALID-SCORE)))

;; Public function: Submit impact report
(define-public (submit-impact-report
    (project-id uint)
    (reporting-period uint)
    (co2-reduced uint)
    (biodiversity-index uint)
    (water-quality-score uint)
    (soil-health-score uint)
    (community-impact-score uint))
    (let 
        ((report-id (var-get report-nonce))
         (auditor-data (map-get? certified-auditors tx-sender)))
        (if (is-some auditor-data)
            (begin
                (map-set impact-reports report-id {
                    project-id: project-id,
                    reporting-period: reporting-period,
                    co2-reduced: co2-reduced,
                    biodiversity-index: biodiversity-index,
                    water-quality-score: water-quality-score,
                    soil-health-score: soil-health-score,
                    community-impact-score: community-impact-score,
                    report-date: stacks-block-height,
                    verified: true,
                    auditor: tx-sender
                })
                (var-set report-nonce (+ report-id u1))
                (ok report-id))
            ERR-AUDITOR-NOT-CERTIFIED)))

;; Public function: Update compliance status
(define-public (update-compliance
    (project-id uint)
    (regulation-type (string-utf8 100))
    (compliance-status bool)
    (violations (list 5 (string-utf8 100)))
    (corrective-actions (list 5 (string-utf8 100))))
    (let ((compliance-id (+ project-id stacks-block-height)))
        (if (is-eq tx-sender (var-get contract-owner))
            (begin
                (map-set compliance-history compliance-id {
                    project-id: project-id,
                    compliance-date: stacks-block-height,
                    regulation-type: regulation-type,
                    compliance-status: compliance-status,
                    violations: violations,
                    corrective-actions: corrective-actions
                })
                (ok compliance-id))
            ERR-NOT-AUTHORIZED)))

;; Public function: Conduct risk assessment
(define-public (assess-project-risk
    (project-id uint)
    (financial-risk uint)
    (environmental-risk uint)
    (operational-risk uint)
    (regulatory-risk uint))
    (let 
        ((overall-risk (calculate-risk-score financial-risk environmental-risk operational-risk regulatory-risk))
         (auditor-data (unwrap! (map-get? certified-auditors tx-sender) ERR-AUDITOR-NOT-CERTIFIED)))
        (if (and 
            (get active auditor-data)
            (>= (get certification-level auditor-data) CERT-ADVANCED))
            (begin
                (map-set risk-assessments project-id {
                    project-id: project-id,
                    financial-risk: financial-risk,
                    environmental-risk: environmental-risk,
                    operational-risk: operational-risk,
                    regulatory-risk: regulatory-risk,
                    overall-risk-score: overall-risk,
                    assessment-date: stacks-block-height
                })
                (ok overall-risk))
            ERR-INSUFFICIENT-QUALIFICATION)))

;; Public function: Dispute audit results
(define-public (dispute-audit
    (audit-id uint)
    (dispute-reason (string-utf8 300)))
    (let 
        ((audit (unwrap! (map-get? project-audits audit-id) ERR-AUDIT-NOT-FOUND))
         (dispute-id (+ audit-id stacks-block-height)))
        (begin
            (map-set audit-disputes dispute-id {
                audit-id: audit-id,
                disputer: tx-sender,
                dispute-reason: dispute-reason,
                dispute-date: stacks-block-height,
                resolution: u"",
                resolved: false,
                resolver: none
            })
            (map-set project-audits audit-id (merge audit {status: STATUS-DISPUTED}))
            (ok dispute-id))))

;; Public function: Resolve audit dispute
(define-public (resolve-dispute
    (dispute-id uint)
    (resolution (string-utf8 300))
    (uphold-audit bool))
    (let ((dispute (unwrap! (map-get? audit-disputes dispute-id) ERR-AUDIT-NOT-FOUND)))
        (if (is-eq tx-sender (var-get contract-owner))
            (begin
                (map-set audit-disputes dispute-id (merge dispute {
                    resolution: resolution,
                    resolved: true,
                    resolver: (some tx-sender)
                }))
                (if uphold-audit
                    (map-set project-audits (get audit-id dispute) 
                        (merge (unwrap-panic (map-get? project-audits (get audit-id dispute))) 
                               {status: STATUS-COMPLETED}))
                    (map-delete project-audits (get audit-id dispute)))
                (ok true))
            ERR-NOT-AUTHORIZED)))

;; Private function: Calculate overall audit score
(define-private (calculate-overall-score (compliance uint) (environmental uint) (social uint))
    (/ (+ (+ (* compliance u40) (* environmental u40)) (* social u20)) u100))

;; Private function: Calculate risk score
(define-private (calculate-risk-score (financial uint) (environmental uint) (operational uint) (regulatory uint))
    (/ (+ (+ (+ financial environmental) operational) regulatory) u4))

;; Private function: Update auditor statistics
(define-private (update-auditor-stats (auditor principal) (score uint))
    (match (map-get? certified-auditors auditor)
        auditor-data
        (let 
            ((completed-audits (get audits-completed auditor-data))
             (current-avg (get average-score auditor-data))
             (new-avg (/ (+ (* current-avg completed-audits) score) (+ completed-audits u1))))
            (map-set certified-auditors auditor (merge auditor-data {
                audits-completed: (+ completed-audits u1),
                average-score: new-avg
            }))
            true)
        false))

;; Read-only function: Get audit details
(define-read-only (get-audit (audit-id uint))
    (map-get? project-audits audit-id))

;; Read-only function: Get impact report
(define-read-only (get-impact-report (report-id uint))
    (map-get? impact-reports report-id))

;; Read-only function: Get auditor certification
(define-read-only (get-auditor-certification (auditor principal))
    (map-get? certified-auditors auditor))

;; Read-only function: Get compliance history
(define-read-only (get-compliance-history (project-id uint))
    (map-get? compliance-history project-id))

;; Read-only function: Get risk assessment
(define-read-only (get-risk-assessment (project-id uint))
    (map-get? risk-assessments project-id))

;; Read-only function: Get audit dispute
(define-read-only (get-audit-dispute (dispute-id uint))
    (map-get? audit-disputes dispute-id))

;; Read-only function: Calculate project sustainability score
(define-read-only (calculate-sustainability-score (project-id uint))
    (match (map-get? impact-reports project-id)
        report
        (let 
            ((environmental-score (/ (+ (+ (get biodiversity-index report) 
                                           (get water-quality-score report)) 
                                        (get soil-health-score report)) u3))
             (social-score (get community-impact-score report))
             (overall-sustainability (/ (+ environmental-score social-score) u2)))
            overall-sustainability)
        u0))

;; Read-only function: Get project audit summary
(define-read-only (get-project-audit-summary (project-id uint))
    (let 
        ((latest-audit (map-get? project-audits project-id))
         (latest-report (map-get? impact-reports project-id))
         (risk-data (map-get? risk-assessments project-id)))
        {
            audit: latest-audit,
            impact-report: latest-report,
            risk-assessment: risk-data,
            sustainability-score: (calculate-sustainability-score project-id)
        }))

