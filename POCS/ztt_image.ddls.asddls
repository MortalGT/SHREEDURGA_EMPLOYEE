@EndUserText.label : 'Image Table'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table ztt_image {

  key client : abap.clnt not null;
  key id     : abap.numc(3) not null;
  text       : abap.char(10);
  pic_url    : abap.char(256);

}
