@EndUserText.label : 'Purchase Order Header'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zrap_po_head {
  key client            : abap.clnt not null;
  key po_uuid           : sysuuid_x16 not null;
  po_id                 : abap.numc(10);
  org                   : abap.char(4);
  status                : abap.char(1);
  created_by            : syuname;
  created_at            : timestampl;
  last_changed_by       : syuname;
  last_changed_at       : timestampl;
  local_last_changed_at : timestampl;
}
