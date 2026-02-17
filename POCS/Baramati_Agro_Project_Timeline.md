# Baramati Agro Project Timeline

## 🔵 OVERALL STRATEGY VALIDATION

**Strategy:**
1.  **Phase 1** → Controlled environment (Owned store, no payment gateway)
2.  **Phase 2** → Add revenue cycle (Secondary sale)
3.  **Phase 3** → Add external complexity (Franchise + partial GRN + credit note)
4.  **Phase 4** → MIS + optimization

This approach reduces integration risk.

---

## 🟢 PHASE 1 – OWNED STORE PRIMARY FLOW (Inbound Only)
**🎯 Goal:** Owned store manager can: Place replenishment order → Receive goods → Complete GRN

### Scope Breakdown (Feature-wise)

#### 1️⃣ Store Setup
*   Store master sync from SAP
*   SKU master sync (with images)
*   Basic pricing sync
*   Available stock visibility (central warehouse)

#### 2️⃣ Replenishment Order
*   Store manager creates order
*   MBQ validation
*   MSL validation
*   Cut-off time validation
*   Region routing logic
*   Order creation in SAP
*   **⚠️ Missing (Added):** Order edit/cancel window before dispatch

#### 3️⃣ Dispatch & Delivery
*   Delivery note visibility
*   Invoice generation (even if internal)
*   Delivery tracking status

#### 4️⃣ Goods Receipt (GRN)
*   SKU-wise quantity confirmation
*   SKU-wise weight capture
*   Basic variance capture
*   GRN posting in SAP
*   **⚠️ Missing (Added):** Tolerance limit logic (auto-approve vs approval)
*   **⚠️ Missing (Added):** Damage capture

### ❗ What NOT in Phase 1
*   Payment gateway
*   Secondary sales
*   Ratings
*   Credit notes automation (only basic variance log)

### ✅ Phase 1 Outcome
Inbound supply chain stabilized.

---

## 🟡 PHASE 2 – OWNED STORE SECONDARY SALE (Revenue Cycle)
**🎯 Goal:** Complete end-to-end lifecycle for owned stores: Purchase → Stock → Sell → Payment → Closure

### Scope Breakdown

#### 1️⃣ POS / Secondary Sale
*   Create sales invoice
*   SKU scan
*   Price auto-fetch
*   Tax calculation
*   Discount rules (basic)

#### 2️⃣ Payment Collection
*   Cash
*   UPI
*   Card
*   Multi-mode split payment
*   **⚠️ Missing (Added):** Refund handling
*   **⚠️ Missing (Added):** Sales return handling

#### 3️⃣ Inventory Impact
*   Stock reduction real-time
*   Stock reconciliation
*   Negative stock prevention

#### 4️⃣ Basic Reporting
*   Daily sales report
*   Store-wise sales
*   Stock on hand

### ✅ Phase 2 Outcome
Owned store is fully live operationally. **(First real go-live milestone)**

---

## 🟠 PHASE 3 – FRANCHISE PRIMARY FLOW
**🎯 Goal:** Franchise ordering to Baramati warehouse + financial adjustments.

### Scope Breakdown

#### 1️⃣ Franchise Onboarding
*   Franchise master
*   Pricing type (franchise pricing)
*   Credit vs prepaid flag

#### 2️⃣ Franchise Order Placement
*   SKU visibility
*   Available stock visibility
*   MBQ validation
*   Region routing
*   Payment rule (if prepaid)
*   **⚠️ Missing (Added):** Order cut-off handling
*   **⚠️ Missing (Added):** Order freeze logic

#### 3️⃣ Dispatch Handling
*   Partial dispatch support
*   Backorder logic
*   Shipment tracking

#### 4️⃣ Partial GRN
*   SKU-wise confirmation
*   Short receipt handling
*   Excess receipt handling
*   Reason codes mandatory

#### 5️⃣ Credit / Debit Note Handling
*   Auto credit note for short supply
*   Weight variance logic
*   Approval workflow for large difference
*   **⚠️ Missing (Added):** Claim management workflow
*   **⚠️ Missing (Added):** Dispute resolution window

#### 6️⃣ Secondary Sale (Reuse Phase 2)
*   Secondary sales same as owned stores.
*   **Difference:** Margin structure, Possibly franchise-specific pricing.

### ✅ Phase 3 Outcome
Full B2B channel stabilized.

---

## 🔴 PHASE 4 – MIS & Governance
**🎯 Goal:** Add governance + controls (not just reporting).

### Scope Breakdown

#### 1️⃣ Operational Dashboards
*   Store sales performance
*   Franchise ordering pattern
*   Delivery adherence
*   GRN variance %

#### 2️⃣ Financial Reports
*   Credit outstanding
*   Invoice aging
*   Margin tracking

#### 3️⃣ Rating & Performance Model
*   Delivery rating scoring
*   Store performance grading

#### 4️⃣ Audit & Controls
*   **⚠️ Missing (Added):** User role matrix
*   **⚠️ Missing (Added):** Audit log
*   **⚠️ Missing (Added):** Fraud prevention (price override log)
*   **⚠️ Missing (Added):** Stock adjustment tracking

---

## 🔎 CRITICAL PROCESSES IDENTIFIED & ASSIGNED

The following critical gaps have been assigned to phases:
*   ❗ **Order cancellation logic** (Phase 1)
*   ❗ **Backorder management** (Phase 3)
*   ❗ **Sales return handling** (Phase 2)
*   ❗ **Refund process** (Phase 2)
*   ❗ **Tolerance-based GRN approval** (Phase 1)
*   ❗ **Inventory reconciliation cycle** (Phase 2)
*   ❗ **Role & authorization matrix** (Phase 4)
*   ❗ **Data sync failure handling** (Throughout/Foundation)
*   ❗ **Offline mode** (If required - To Be Decided)
*   ❗ **Error logging & retry framework** (Throughout/Foundation)

---

## 🗂️ Feature Distribution Summary
*   **Phase 1:** ~8–10 features
*   **Phase 2:** ~6–8 features
*   **Phase 3:** ~10–12 features
*   **Phase 4:** ~8–10 features
*   **Total:** 30+ controlled features.

---

## 📅 REALISTIC TIMELINE

**Assumptions:**
*   1 Backend Team
*   1 SAP Team
*   1 Frontend Team
*   Proper parallel development

### Timeline Breakdown

**Phase 1 – 6 to 8 weeks**
*   Internal testing + pilot store

**Phase 2 – 4 to 6 weeks**
*   Revenue cycle integration
*   👉 **First realistic Go-Live (Owned Stores Full):** 10–12 weeks from project start

**Phase 3 – 8 to 10 weeks**
*   Franchise complexities + credit note logic
*   👉 **Franchise Go-Live:** 20–22 weeks total

**Phase 4 – 4 weeks**
*   Dashboards + stabilization

### 🏁 Final Realistic Timeline Overview

| Milestone | Timeline |
| :--- | :--- |
| **Owned Store Inbound** | 2 months |
| **Owned Store Full** | 3 months |
| **Franchise Live** | 5–6 months |
| **Complete Stabilized System** | 6 months |

*If rushed → risk of rework.*
