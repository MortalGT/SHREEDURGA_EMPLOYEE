# Effort Estimation: JIWARJIKA ASRS Integration

## Project Overview
**Client:** JIWARAJKA TEXTILE INDUSTRIES
**Scope:** Integration between SAP and Automated Storage and Retrieval System (AD-WCS).
**Modules:** MM (Inventory Management), PP (Production Planning), SD (Sales & Distribution - Delivery).

---

## Technical Effort Estimation

### 1. Database & Dictionary Objects
| Object Name / Description | Type | Functional Requirement Ref. | Tech Effort (Mandays) |
| :--- | :--- | :--- | :--- |
| **ZASRS_LOG_HDR / ITM** | Tables | Interface Logging (Request/Response) | 0.5 |
| **ZASRS_PALLET_HDR / ITM** | Tables | Store Pallet/Carton linkage in SAP (Interim) | 0.5 |
| **ZASRS_CONSTANTS** | Table | Configuration for API URLs, Plants, SLocs | 0.5 |
| **CDS Views (Interface)** | Core Data Services | Data extraction for APIs | 1.0 |
| **Subtotal** | | | **2.5 Days** |

### 2. Interface Development (API / OData)
| Interface Name | Direction | Functional Requirement Ref. | Tech Effort (Mandays) |
| :--- | :--- | :--- | :--- |
| **SKU Master Data API** | Outbound (SAP -> WCS) | Section 3: SKU Master Data | 1.0 |
| **Carton Details API** | Outbound (SAP -> WCS) | Section 4: Carton Details | 1.5 |
| **Palletization Conf API** | Inbound (WCS -> SAP) | Section 5: Palletization Confirmation | 1.5 |
| **Store-In Confirmation API** | Inbound (WCS -> SAP) | Section 6: Store-In Process | 2.0 |
| **QC Status Update API** | Outbound (SAP -> WCS) | Section 7: QC Status Handling | 1.0 |
| **Store-Out Pick Instruction** | Outbound (SAP -> WCS) | Section 9.1: Store-Out Process | 1.0 |
| **Dispatch Confirmation API** | Inbound (WCS -> SAP) | Section 9.2: Store-Out Dispatch | 2.0 |
| **Subtotal** | | | **10.0 Days** |

### 3. Business Logic & Background Processing
| Component | Description | Functional Requirement Ref. | Tech Effort (Mandays) |
| :--- | :--- | :--- | :--- |
| **Stock Transfer Logic** | Implement 311 Movement (Staging -> ASRS) on Store-In | Section 6: ASRS Location Logic | 1.0 |
| **Delivery Creation Logic** | Auto-create Outbound Delivery from Cockpit selection | Section 8: Delivery Creation | 1.5 |
| **Delivery Update & PGI** | Update Delivery with Batch Split & Post Goods Issue on Dispatch Conf | Section 8: Delivery Update & PGI | 1.5 |
| **Exception Handling** | Error logging, re-processing logic for failed API calls | General | 1.0 |
| **Subtotal** | | | **5.0 Days** |

### 4. Frontend / User Interface

| Application                | Description                                                       | Functional Requirement Ref.  | Tech Effort (Mandays) |
| :------------------------- | :---------------------------------------------------------------- | :--------------------------- | :-------------------- |
| **ASRS Store-Out Cockpit** | Dashboard to view ASRS stock, select Batches/Pallets for Dispatch | Section 8: Store-Out Process | 5.0                   |
| **Interface Monitor**      | Report to view API Logs (Req/Res) and Status                      | General                      | 1.0                   |
| **Subtotal**               |                                                                   |                              | **6.0 Days**          |

---

## Functional Effort Estimation

### 1. Functional Specification & Design(SIT / UAT Support)

| Activity | Description | Effort (Mandays) |
| :--- | :--- | :--- |
| **FS Preparation** | Writing detailed Functional Specs for all 7 interfaces & Cockpit | 5.0 |
| **Process Mapping** | Defining specific Movement Types, Storage Locations, and Picking logic | 2.0 |
| **Test Data Creation** | Creating Material Master, Batches, Orders for testing | 2.0 |
| **Subtotal** | | **9.0 Days** |


---

## Summary of Efforts

| Category                              | Effort (Mandays) |
| :------------------------------------ | :--------------- |
| **Technical Development**             | **23.5**         |
| **Functional Design & Testing**       | **9.0**          |
| **Project Management / Buffer (10%)** | **3.5**          |
| **TOTAL ESTIMATED EFFORT**            | **36.0 Days**    |

---

### Assumptions for Estimation
1.  **Connectivity:** Assuming direct REST or configured connections availability.
2.  **Standard BAPIs:** Standard SAP BAPIs (Good Mvt, Delivery Create/Update) are fit for purpose without major enhancements.
3.  **Clarifications:** The open points in the spec (QC timing, etc.) are resolved without major process changes.
