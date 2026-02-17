@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View for PO Item'
define view entity ZI_RAP_PO_Item
  as select from zrap_po_item
  association to parent ZI_RAP_PO_Head as _Header on $projection.PoUuid = _Header.PoUuid
{
  key item_uuid             as ItemUuid,
      po_uuid               as PoUuid,
      item_id               as ItemId,
      material              as Material,
      @Semantics.quantity.unitOfMeasure: 'Unit'
      quantity              as Quantity,
      unit                  as Unit,
      @Semantics.amount.currencyCode: 'Currency'
      price                 as Price,
      currency              as Currency,
      @Semantics.user.createdBy: true
      created_by            as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at            as CreatedAt,
      @Semantics.user.lastChangedBy: true
      last_changed_by       as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at       as LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,

      /* Associations */
      _Header
}
