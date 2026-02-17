# ASRS – SAP Integration Specification
## Project: JIWARAJKA TEXTILE INDUSTRIES

---

## 1. Introduction
This document outlines the integration specifications between the SAP system and the Automated Storage and Retrieval System (AD-WCS) for JIWARAJKA TEXTILE INDUSTRIES. It details the assumptions, interface contracts, data payloads, and process flows for the palletization and storage operations.

---

## 2. Palletization Process – Assumptions & Interfaces

### 2.1 Assumptions
> [!IMPORTANT]
> The following assumptions are critical for the successful integration of the systems.

1.  **Communication Protocol:** Communication between AD-WCS and the SAP system will be established via REST APIs.
2.  **Barcode Standards:** Pallet and carton barcodes must be unique, follow a predefined format, and must not exceed 14 characters in length.
3.  **Pallet Identification:** A predefined prefix will be added to every pallet barcode to identify the pallet type:
    *   `IDY`
    *   `FDY`
    *   `DTY`
    *   `POY`
4.  **Label Printing:** The generating and printing of pallet and carton barcodes are existing operational processes at the client location and are **out of scope** for this integration.

### 2.2 Palletization Process Flow
1.  **Packing:** Material is packed into cartons at the production line.
2.  **Labeling:** Carton barcodes are generated and printed.
3.  **Palletization:** An operator groups cartons onto a pallet.
4.  **Assignment:** A unique pallet barcode is assigned and affixed to the pallet.

---

## 3. SKU Master Data Interface
**Direction:** SAP ➜ AD-WCS

### Purpose
To synchronize SKU master data required for defining palletization rules and determining ASRS storage logic.

### Endpoint Details
*   **Provider:** AD-WCS
*   **Consumer:** SAP

### Data Contract

| Field Name | Description | Data Type | Length | Notes |
| :--- | :--- | :--- | :--- | :--- |
| **SKU** | Unique Material / SKU Code | String | 15 | Primary Key |
| **SKU Description** | Description of the SKU | String | 100 | |
| **Net Weight** | Weight per SKU unit | String | 5 | |
| **Unit of Measure** | Base Unit of Measure | String | 5 | Defaults to 'KG' if not maintained |
| **Classification** | Velocity Code (A/B/C) | String | 5 | Fast / Medium / Slow |
| **Stack Count** | Maximum cartons per pallet | Integer | 3 | Controls pallet building logic |
| **Weight Tolerance** | Allowed weight deviation | Integer | 5 | In Grams |

> [!NOTE]
> **Functional Impact:**
> *   **Classification** determines the storage priority within the ASRS.
> *   **Stack Count** dictates the logic for building full pallets.

---

## 4. Carton Details Interface
**Direction:** SAP ➜ AD-WCS

### Purpose
To transmit carton-level packing and production data from SAP to AD-WCS for tracking.

### Endpoint Details
*   **Provider:** AD-WCS
*   **Consumer:** SAP

### Data Contract

#### Header: Carton Production

| Field Name | Description | Data Type | Length | Notes |
| :--- | :--- | :--- | :--- | :--- |
| **SKU** | Material / SKU Code | String | 15 | |
| **Batch** | Lot Number + Grade | String | 15 | |
| **Pack Type** | Packaging Type | String | 20 | e.g., Box, Wooden Pallet |

#### Items: Carton Details (Repeatable)

| Field Name | Description | Data Type | Length | Notes |
| :--- | :--- | :--- | :--- | :--- |
| **Carton Barcode** | Unique Carton Identifier | String | 15 | Global Unique Identifier (GUID) equivalent for the plant |
| **Net Weight** | Weight of the carton | Integer | 5 | |
| **Packing Date** | Date of packaging | Date | - | Format: YYYY-MM-DD |

> [!NOTE]
> *   A single batch may be associated with multiple carton identifiers.
> *   Carton barcodes must be unique across the entire plant to prevent scanning conflicts.

---

## 5. Palletization Confirmation Interface
**Direction:** AD-WCS ➜ SAP

### Purpose
To confirm the completion of the palletization process and establish the linkage between cartons and the parent pallet.

### Endpoint Details
*   **Provider:** SAP
*   **Consumer:** AD-WCS

### Data Contract

| Field Name | Description | Data Type | Length |
| :--- | :--- | :--- | :--- |
| **Pallet Barcode** | Unique Pallet Identifier | String | 15 | Includes Type Prefix |
| **Carton Barcodes** | List of Associated Cartons | Array[String] | - | |

---

## 6. Store-In Process Flow (ASRS Inbound)

### Workflow
1.  **Physical Storage:** The pallet is physically stored in the ASRS location.
2.  **Confirmation Trigger:** AD-WCS triggers the completion event.
3.  **Data Transmission:** AD-WCS sends the Store-In confirmation payload to SAP via REST API.

### Interface: Store-In Confirmation
**Direction:** AD-WCS ➜ SAP

**Trigger:** Successful physical storage of the pallet.

**Payload:**
*   Pallet Barcode
*   List of Carton Barcodes
*   SKU
*   Batch
*   Quantity
*   Rack Location (ASRS Coordinate)

### 7. ASRS Location & Stock Movement Logic

**1. ASRS Location Management in SAP**
*   **Decision:** A **Common ASRS Storage Location** will be created in SAP to represent the entire ASRS stock.
*   Individual ASRS rack/bin locations will **not** be mirrored as separate storage locations in SAP.

**2. Stock Movement Logic**
*   **Trigger:** Upon Store-In confirmation from AD-WCS.
*   **Action:** SAP will perform a stock transfer from the Staging location to the Common ASRS Storage Location.
*   *Requirement:* Define the specific Movement Type (e.g., 311 for SLoc to SLoc transfer).


---

## 7. QC Status Handling (Reporting Only)
**Direction:** SAP ➜ AD-WCS

### Purpose
To update AD-WCS with the Quality Control (QC) status of materials stored in the ASRS system.

> [!CAUTION]
> **Scope Limitation:** AD-WCS will **NOT** implement operational blocking logic based on QC status. This data is for **reporting and visibility purposes only**.

### Data Contract

| Field Name | Description | Data Type | Length |
| :--- | :--- | :--- | :--- |
| **SKU** | Material / SKU Code | String | 15 |
| **Production Date** | Date of Production | String | 15 |
| **QC Status** | Quality Status Indicator | String | - | e.g., OK / Not OK |
| **QC Reason** | Reason for Status | String | 100 | Optional |

### Open Clarification Points – QC Process

**1. Timing of Inspection**
*   At which stage is the QC inspection performed?
    *   Pre-Palletization?
    *   Post-Palletization (Before Store-In)?
    *   Post-Store-In (Inside ASRS)?

---

## 8. Store-Out Process – Design Flow (Outbound)

### Workflow Overview
1.  **Cockpit Selection (SAP):**
    *   A custom cockpit in SAP displays batch-wise stock currently in the ASRS.
    *   The user selects "Probable" pallets and batches for dispatch.
2.  **Delivery Creation:**
    *   An Outbound Delivery is created in SAP.
    *   *Constraint:* Quantity is maintained at the item level; no batch split is performed at this stage.
3.  **Instruction Transmission (SAP ➜ AD-WCS):**
    *   SAP sends the Delivery details along with the "Probable" pallet and batch information to the ASRS.
4.  **Physical Picking (ASRS):**
    *   The ASRS system executes the physical retrieval.
    *   *Note:* The actual stock picked may differ from the "Probable" selection based on real-time availability.
5.  **Confirmation (AD-WCS ➜ SAP):**
    *   ASRS sends the "Actual" picked pallet and batch details back to SAP.
6.  **Delivery Update & PGI:**
    *   SAP updates the Delivery with the actual batch split.
    *   Pallet details are recorded.
    *   Post Goods Issue (PGI) is triggered.

---

## 9. Store-Out Interface Contracts

### 9.1 Pick Instruction (Probable)
**Direction:** SAP ➜ AD-WCS

**Header Level**
*   Delivery Number
*   Plant / Storage Location / Shipping Point
*   Dispatch Date
*   Reference ID (UUID)

**Item Level**
*   Delivery Item Number
*   Material Code
*   Required Quantity & UoM

**Probable Allocation Details**
*   Pallet ID
*   Batch Number
*   Target Quantity per Pallet

### 9.2 Dispatch Confirmation (Actual)
**Direction:** AD-WCS ➜ SAP

**Header Level**
*   Delivery Number
*   Dispatch Timestamp
*   ASRS Transaction ID
*   Reference ID (UUID)

**Item Level**
*   Delivery Item Number
*   Material Code

**Actual Pick Details**
*   Pallet ID
*   Batch Number
*   Picked Quantity & UoM

**Status & Error Handling**
*   **Pick Status:** `Success` / `Partial` / `Failed`
*   **Error Code:** Standardized error codes (if applicable)
*   **Error Message:** Descriptive error text
