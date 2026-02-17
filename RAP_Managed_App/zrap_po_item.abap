@EndUserText.label : 'Purchase Order Item'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zrap_po_item {
  key client            : abap.clnt not null;
  key item_uuid         : sysuuid_x16 not null;
  po_uuid               : sysuuid_x16 not null;
  item_id               : abap.numc(6);
  material              : abap.char(10);
  quantity              : abap.quan(13,3);
  unit                  : abap.unit(3);
  price                 : abap.curr(15,2);
  currency              : abap.cuky(5);
  created_by            : syuname;
  created_at            : timestampl;
  last_changed_by       : syuname;
  last_changed_at       : timestampl;
  local_last_changed_at : timestampl;
}
