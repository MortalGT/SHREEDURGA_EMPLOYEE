@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View for PO Header'
define root view entity ZI_RAP_PO_Head
  as select from zrap_po_head
  composition [0..*] of ZI_RAP_PO_Item as _Item
{
  key po_uuid               as PoUuid,
      po_id                 as PoId,
      org                   as Org,
      status                as Status,
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
      _Item
}
