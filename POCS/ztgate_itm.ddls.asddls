@EndUserText.label : 'Gate Entry Item Table'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table ztgate_itm {
  key client : abap.clnt not null;
  key zgate  : abap.char(10) not null;
  key zpos   : abap.numc(6) not null;
  matnr      : abap.char(18);
  werks      : abap.char(4);
  lgort      : abap.char(4);
  menge      : abap.quan(13,3);
  meins      : abap.unit(3);
}
