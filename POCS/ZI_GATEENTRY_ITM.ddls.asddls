@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Gate Entry Item CDS View'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_GATEENTRY_ITM as select from ztgate_itm
  association to parent ZI_GATEENTRY_hdr as _Header on $projection.Zgate = _Header.Zgate
{
  key zgate as Zgate,
  key zpos as Zpos,
  matnr as Matnr,
  werks as Werks,
  lgort as Lgort,
  menge as Menge,
  meins as Meins,

  _Header // Make association public
}
