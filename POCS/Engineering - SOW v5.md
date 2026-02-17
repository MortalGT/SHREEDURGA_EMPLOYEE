
## 1. DOCUMENT CONTROL

| Item                   | Description                                       |
| ---------------------- | ------------------------------------------------- |
| Customer               | Baramati Agro                                     |
| Implementation Partner | Expound Technivo Pvt. Ltd.                        |
| Engagement Type        | Software Engineering & System Integration         |
| ERP System             | S4 HANA                                           |
| Reference Documents    | Business & Process Requirements dated 16 December |
| Delivery Methodology   | Agile with gated milestones and formal sign-offs  |
| Document Version       | 1.7                                               |
| Document Status        | Draft for Review                                  |

---

## 2. INTRODUCTION & CONTEXT

Baramati Agro operates a complex distribution ecosystem involving franchises, owned stores, sales teams, factories, logistics partners, and end customers. Current operations require tighter control, improved visibility, reduced manual intervention, and stronger integration with SAP to ensure financial and inventory accuracy.

This Statement of Work defines the scope for designing, engineering, integrating, testing, and deploying a unified digital platform that enables Baramati Agro to manage franchise ordering, billing, inventory movement, delivery confirmation, secondary sales, and reporting, with SAP ECC6 acting as the system of record.

---

## 3. OBJECTIVES OF THE ENGAGEMENT

The objectives of this engagement are as follows:

- Digitise franchise and owned-store operations end-to-end
- Ensure all inventory movements are reflected accurately in SAP
- Enforce payment discipline across all applicable sales flows
- Provide real-time visibility to sales, factory, logistics, and management teams
- Reduce manual errors and reconciliation effort
- Establish a scalable platform capable of supporting future growth

---

## 4. Functional Discussion

> Note:
> Any section marked as **“Discussion Required”** indicates dependency on business / SAP / operations sign-off.
> Absence of a final decision shall not be treated as a product or delivery gap.

---

### 4.1. Outlet / Dealer Types

#### 4.1.1 Franchise Store
- Independently owned
- Uses Franchise App
- Procures stock from Baramati

**Capabilities**
- Place primary orders
- Accept delivery & GRN
- Perform secondary billing
- Accept customer payments (Primary Order Payments)

**The below points are covered in the scope and will be discussed in the initial phase of the project**
- Final list of franchise roles and permissions (Owner vs Staff)
- Whether partial GRN is allowed or strictly full GRN
	- internal comment (makrand): partial grn and balance will be treated as shrinkage/credit note (As per given format).
- Whether franchise credit is allowed in future (currently assumed NO)

---

#### 4.1.2 Baramati Owned Store
- Owned by Baramati
- Stock movement is internal

**Capabilities**
- Raise stock requests
- Perform internal transfers (STO)
- Perform secondary billing
- No primary payment flow

##### Internal Transfer
Internal Transfer refers to stock transfer between the company and its own stores.
This will be treated as an internal stock movement and no external customer or third party is involved in this process.

##### Customer Payment to Baramati
End customers will make the payment directly at the Application.
No payment request or payment initiation will be triggered from SAP.
SAP will only capture the sales transaction data for accounting and reporting purpose.

**The below points are covered in the scope and will be discussed in the initial phase of the project**
- Identification logic of “Baramati Owned Store” (flag in SAP / backend)
- Whether owned stores can ever convert STO → SO (future)
- Whether owned store secondary billing follows exact franchise rules.

---

#### 4.1.3 Distributor / Stockist (Edge Case)
- Used only for stock procurement
- No billing responsibility

**Capabilities**
- Manual stock adjustment only
- Only Baramati SKUs allowed

**Explicit Out of Scope**
- Distributor billing workflows
- Distributor payment tracking
- Distributor reconciliation

> This flow is informational and stock-only.  
> No automation commitment is assumed.

#### 4.1.4 Material Types
Following material types are considered as part of scope:
a. Baramati Stock
b. Third-Party Stock (e.g. Coke)
GRN will be created only for Baramati stock for inventory tracking in SAP.
GRN creation for third-party stock will not be required in SAP.

---

### 4.2. Billing Types

#### 4.2.1 Primary Billing
**Definition:** Billing between Franchise and Baramati

**Constraints**
- Payment is mandatory
- SAP Sales Order required
- Invoice must be generated before delivery
- Payment gateway mandatory - Virtual
- No manual override allowed

**The below point is covered in the scope and will be discussed in the initial phase of the project**
- Whether credit limits will ever be enabled

---

#### 4.2.2 Secondary Billing
**Definition:** Retail billing to end customers

**Constraints**
- Customer phone number mandatory
- Pricing driven from SAP-primary (Secondary in New System)
- Payment must be captured
- Invoice must be generated
- Discount logic controlled by franchise rules

**Secondary Sales Pricing**
Pricing for secondary sales will not be maintained in SAP.
Pricing will be provided from source system and can be:
- SKU-wise, or
- Customer-wise & SKU-wise (in specific cases).

**Store-wise handling:**
- **Baramati-owned stores:**
  Mode of payment will be mapped to a predefined (dummy) customer in SAP.
- **Franchisee stores:**
  Mode of payment will not be captured in SAP.

**The below point is covered in the scope and will be discussed in the initial phase of the project**
- GST handling at franchise vs Baramati level

---

### 4.3. Sales Flow Definitions (Total: 4)

**Payment-Based Sales Order Clubbing**
Sales Orders will be created/clubbed based on the mode of payment.
API integration will create sales orders in SAP accordingly, with below rule:
1 Sales Order = 1 Payment Mode

#### FLOW 1: Primary Billing – Baramati → Franchise

##### Purpose
Enable franchises to procure stock from Baramati.

##### Preconditions
- Franchise is active
- SKU master synced from SAP
- Pricing available in SAP
- Credit / advance balance synced

> Any failure in preconditions will block order placement.

##### Functional Flow
1. Franchise logs into Franchise App
2. Views SKU list with:
   - Images
   - Available quantity
   - SAP price
3. Places order within allowed time window
4. System validates:
   - MBQ / MSL (internal comment(makrand): sap will not be responsible for it. it can be incorporated in frontend system speak with shreedhar)
   - Region-based routing
   - Price from SAP
5. Franchise completes payment via gateway - Virtual
6. Backend creates Sales Order in SAP
7. SAP generates invoice
8. Delivery scheduled (next day)
9. Delivery confirmation:
   - OTP verification
   - SKU-wise quantity & weight confirmation
10. GRN auto-posted
11. Shortage/damage handling:
   - Approval workflow
   - Credit note creation (internal (makarand): need custom report to track billed quantity, received quantity, billing amount, customer wise. we can provide filters )

##### Constraints
- ❌ Order without payment not allowed
- ✔ Credit note only after approval
- ✔ OTP mandatory for delivery confirmation

**The below points are covered in the scope and will be discussed in the initial phase of the project**
- Exact approval hierarchy for shortage/damage
- SLA for approval beyond 24 hours (internal comment (makarand): which sla and for what? reporting of shortage and damange)
- Whether partial delivery confirmation is allowed

---

#### FLOW 2: Secondary Billing – Franchise → End Customer

##### Purpose
Enable retail sales at franchise stores.

##### Preconditions
- Stock available
- Pricing synced from SAP (internal comments (makarand): pricing will not be synced from sap already discussed)

##### Functional Flow
1. Franchise selects SKU
2. Enters customer details:
   - Phone number mandatory
   - Name optional
3. Applies:
   - Discounts (percentage or absolute)
   - Delivery charges (optional)
4. Customer makes payment:
   - QR
   - Card (will be entered via razorpay link)
   - Cash (via gateway)
5. Invoice generated
6. Invoice sent via WhatsApp / SMS
7. Thank-you message with rating link sent

##### Constraints
- ✔ Discount logic configurable per franchise (internal comment (makrand): this will be managed baramati as pricing is also managed by baramati and no control to be given to customer)
- ✔ All invoices must be stored and auditable

**The below points are covered in the scope and will be discussed in the initial phase of the project**
- Max discount limits
- Loyalty / repeat-customer logic
- customer invoices sync back to SAP for own stores

---

#### FLOW 3: Baramati Owned Store – Internal Sale (STO)

##### Purpose
Handle internal stock movement for owned stores.

##### Preconditions
- Store marked as "Baramati Owned"
- SAP integration active

##### Functional Flow
1. Store raises stock request
2. System creates STO in SAP
3. No payment step
4. Delivery executed
5. GRN confirmation completed

##### Constraints
- ✔ Only STO allowed
- ✔ Internal accounting handled in SAP

**Explicit Assumption**
- STO lifecycle (delivery → posting) is controlled fully by SAP.
- Backend will only reflect status.

---

#### FLOW 4: Secondary Sale at Baramati Owned Store (Manual Clubbing)

##### Purpose
Handle retail billing at owned stores with SAP limitation.

##### Functional Flow
1. Orders punched in Franchise App
2. Baramati user clubs orders
3. One-time dummy customer created
4. Sales Order Automatically created in SAP
5. Delivery & invoice Automatically executed
6. Secondary invoice issued to customer

##### Constraints
- ✔ Used only for owned stores
- ✔ Dummy customer must be flagged

**Explicit Out of Scope**
- Automation of clubbing logic
- Auto-creation of dummy customers
- Auto-reconciliation of these orders

---


### 4.4. SAP Integration Rules

#### SAP as System of Record
- Pricing
- Inventory
- Customer master
- Invoices
- Credit notes

#### Backend → SAP
- Sales Order
- STO
- GRN
- Credit Notes
- Payments on account

#### Constraints
- ✔ Billing executed Automatically in SAP
- ✔ Failed API calls logged and retried

**Explicit Limitation**
- SAP failures or delays are outside backend accountability.

---

### 4.5. Exception Handling

#### Shortage / Damage
- Proof mandatory
- Approval required within 24 hours
- Credit note issued post approval

**The below points are covered in the scope and will be discussed in the initial phase of the project**
- Proof formats (image/video)
- Multi-level approval rules

---

#### Zero Bill Stores
- Highlighted daily & monthly
- Visible to sales team

**The below points are covered in the scope and will be discussed in the initial phase of the project**
- Definition of “zero bill” threshold
- Escalation ownership

---

### 4.6. Compliance & Audit Constraints

- All transactions logged
- OTP verification mandatory for delivery
- No manual stock edits without audit trail
- Role-based access enforced

**Explicit Assumption**
- Audit policies and retention period governed by Baramati IT/SAP policy.

---

### 4.7. Final Summary

#### Sales Flows
- **4 Functional Sales Flows**

#### Billing Types
- **2 Billing Types**
  - Primary
  - Secondary

#### Outlet Types
- **3 Operational Types**
  - Franchise
  - Baramati Owned
  - Distributor (stock-only)

---

> Any functionality not explicitly defined here is **not assumed** and will require a separate discussion or change request.

---
## 5. Extended Functional Clarifications (From Meeting Notes)

---

### 5.1 Primary Billing – Extended Operational Rules

**Order Cut-off & Timing**
- Primary orders are placed via Franchise Application at **6:00 PM** (internal comment (makarand): majorly order is placed between 2-3pm check time)

**Additional Functional Requirements**
1. Franchise must be able to:
   - View all SKUs with images
   - View system-available quantity
2. Order placement validations must include:
   - MBQ (Minimum Billing Quantity)
   - MSL (Minimum Stock Level)
   - Region-based order directing
   - Pricing sourced from ERP
3. Payment must be completed via payment gateway- virtual
4. Invoice must be generated and shared with franchise
   - Invoice must be available with the delivery vehicle
5. Morning delivery confirmation must capture:
   - SKU-wise quantity confirmation
   - SKU-wise weight confirmation
   - OTP verification
   - Delivery rating on:
     - Timeliness
     - Behaviour
     - Hygiene

**The below points are covered in the scope and will be discussed in the initial phase of the project**
- Delivery rating scoring model and scale
- Ownership and usage of delivery ratings (analytics vs enforcement)

---

### 5.2 GRN & Quantity Difference Handling

- GRN must be auto-linked with OTP verification
- Quantity mismatch handling:
  - System-detected differences only
  - Automatic initiation of credit note flow

**Explicit Constraint**
- Franchise users cannot manually override received quantity without triggering audit logs.

---

### 5.3 Damage Reporting & Approval Workflow

**Process**
1. Franchise reports damage with supporting proof
2. Approval required before processing
3. Checker reviews within 24 hours
4. Approval via:
   - OTP, or
   - Login/password

**Eligible Approvers**
- QA
- Sales Manager
- Sales Officer
- BU Head

**Post Approval**
- Credit note issued against original invoice

**The below points are covered in the scope and will be discussed in the initial phase of the project**
- Proof formats (image/video limits)
- Single vs multi-level approval logic

---

## 6. Secondary Billing – Extended Functional Rules

### 6.1 Discount & Charges Handling

- Franchise may apply:
  - Discount coupons
  - Delivery charges
- Discounts can be:
  - Percentage-based
  - Absolute-value based
- Applicable to:
  - Franchise stores
  - Baramati owned stores with BU head approval

**Explicit Constraint**
- Discount configuration is managed by franchise/business
- Backend will not enforce discount policies unless explicitly configured

---

### 6.2 Customer Communication

- Invoice sent via WhatsApp or SMS
- Thank-you message includes rating link

**The below points are covered in the scope and will be discussed in the initial phase of the project**
- Rating scoring model
- Usage of ratings for analytics or incentives

---

## 7. Marketing & Promotions

### 7.1 Primary Billing Marketing Rules

- Primary billing schemes are managed by Marketing Team in ERP
- Scheme of the day must be updated in ERP

**Explicit Assumption**
- Backend will only consume scheme data from ERP
- Scheme definition and validation are ERP responsibilities

---

### 7.2 Secondary Billing Marketing Rules

- Schemes applicable to secondary billing are managed in ERP
- Targeted discounts/messages for frequent customers:
  - Logic based on customer phone number
  - Derived from order history

**The below points are covered in the scope and will be discussed in the initial phase of the project**
- Frequency threshold definition
- Discount eligibility logic

---

### 7.3 Promotional Material Tracking

- Promotional material delivered to franchise
- Delivery confirmation required

**The below points are covered in the scope and will be discussed in the initial phase of the project**
- Confirmation mechanism (manual vs OTP vs acknowledgement)
- Impact of non-confirmation

---

## 8. Logistics & Delivery Extensions

### 8.1 GRN-Level Logistics Data

At time of GRN, dealer must enter:
- Receiving temperature
- Receiving time

**The below points are covered in the scope and will be discussed in the initial phase of the project**
- Temperature units and acceptable range
- Enforcement logic for out-of-range values

---

### 8.2 Crate Management

- Capture crates received vs crates returned

**The below points are covered in the scope and will be discussed in the initial phase of the project**
- Ownership of crates
- Reconciliation logic
- Financial impact, if any

---

## 9. Customer Care

### 9.1 Ticketing

- Franchise users (including owned stores) can:
  - Raise support tickets
  - Upload images
- Ticketing model:
  - Bucket-based
  - Tag-driven categorization

**Explicit Assumption**
- SLA definition and resolution workflows are outside current scope

---

## 10. MIS & Reporting

### 10.1 Sales MIS

- Sales officer access limited to assigned plants
- Orders automatically attached to mapped plants
- Reports required:
  - Store-wise sales targets vs achieved
  - SKU-wise and category-wise performance
  - Zero-bill stores highlighted daily and monthly
  - Owned store’s – Automatically day end report 1) Dailly settlement report 2) Daily SKU wise sales report

**The below points are covered in the scope and will be discussed in the initial phase of the project**
- Target source system
- Escalation and notification logic

---

### 10.2 Factory MIS

- SKU-wise sales order summary
- Customer-wise sales order summary

**Explicit Assumption**
- Report formats to be finalized jointly during design phase

---

## 11. Franchise Application – Stock Adjustments

- Franchise can manually stock up when buying from Baramati distributor/stockist
- Allowed only:
  - In cold-chain or emergency cases
  - Against Baramati SKUs only

**Explicit Constraint**
- No distributor billing automation included

---

## 12. Online / Aggregator Billing (Future Scope)

- Handling Swiggy / direct online orders

**The below points are covered in the scope and will be discussed in the initial phase of the project**
- Order redirection logic
- Franchise assignment rules
- Settlement and reconciliation ownership

**Explicit Out of Scope**
- Aggregator integrations
- Automated settlement flows

---

## 13. Forecasting

- Forecasting functionality to be evaluated after sufficient data availability

**Explicit Out of Scope**
- Forecasting models
- Demand prediction algorithms
- Auto-replenishment

---

## 14. FOOD 365 & On-the-Go Sales

- Same functional logic as secondary billing
- Differences:
  - Product catalog differs
  - Billing occurs before payment

**The below points are covered in the scope and will be discussed in the initial phase of the project**
- Compliance implications of bill-before-payment
- Exception handling for payment failure

---

> Any functionality not explicitly defined here is **not assumed** and will require a separate discussion or change request.

---

## 15. Project Timeline

### 🔵 Phase 1 – Owned Store (Ordering & Receiving Only)

**Duration:** 2 Months

First, we stabilize the basic supply flow.

**Owned store manager should be able to:**
- View SKUs and stock
- Place replenishment order
- Follow cut-off rules
- Edit/cancel order before dispatch
- Receive goods
- Do SKU-wise GRN
- Capture quantity difference
- Handle basic variance with reason codes

No sales yet. No payment gateway.
Just a smooth inward supply process.

**👉 End of Phase 1:**
Owned store can order and receive goods properly.

### 🟢 Phase 2 – Owned Store Full Operations (Start Sales)

**Duration:** 1 Month

Now we complete the store lifecycle.

**We add:**
- Secondary sales (billing / POS)
- Multiple payment modes (cash, UPI, card)
- Inventory auto-reduction
- Sales return handling
- Daily sales summary
- Basic stock reconciliation

**👉 End of Phase 2 (Month 3):**
Owned store is fully live (Purchase + Sales).

This is your first major go-live milestone.

### 🟠 Phase 3 – Franchise Rollout

**Duration:** 1.5 Months

Now we extend the model to franchise.

**We implement:**
- Franchise onboarding
- Franchise pricing logic
- Order placement to warehouse (Baramati)
- Order cut-off validation
- Partial dispatch handling
- Partial GRN
- Short/excess handling
- Credit note / debit note for differences
- Basic dispute handling

Secondary sales logic remains the same as owned stores.

**👉 End of Phase 3 (Month 4.5):**
Franchise ordering and supply fully functional.

### 🔴 Phase 4 – MIS & Control Layer

**Duration:** 0.5 Month (2 Weeks)

Now we strengthen control and visibility.

**We add:**
- Sales dashboards
- GRN variance reports
- Credit outstanding report
- Store performance tracking
- Delivery rating model
- Role & access control
- Audit logs

**👉 End of Phase 4 (Month 5):**
System is fully stabilized with monitoring and governance.

### 📌 5-Month Timeline Summary

| Phase | Duration | What Goes Live |
| :--- | :--- | :--- |
| Phase 1 | 2 Months | Owned Store Ordering + GRN |
| Phase 2 | 1 Month | Owned Store Full Sales |
| Phase 3 | 1.5 Months | Franchise Supply Live |
| Phase 4 | 0.5 Month | MIS + Control |

**Total:** 5 Months