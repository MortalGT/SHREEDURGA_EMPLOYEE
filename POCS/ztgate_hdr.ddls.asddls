@EndUserText.label : 'Gate Entry Header Table'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table ztgate_hdr {
  key client : abap.clnt not null;
  key zgate  : abap.char(10) not null;
  plant      : abap.char(4);
  gateindt   : abap.dats;
  gateinout  : abap.tims;
  gateoutdt  : abap.dats;
  gateoutout : abap.tims;
  vehno      : abap.char(20);
  oname      : abap.char(35);
  drvlicen   : abap.char(20);
  remark     : abap.char(255);
  bill       : abap.char(20);
  tare       : abap.quan(13,3);
  gross      : abap.quan(13,3);
  net        : abap.quan(13,3);
  pack       : abap.char(20);
  chk        : abap.char(1);
  mblnr      : abap.char(10);
  gjahr      : abap.numc(4);
  bukrs      : abap.char(4);
}
