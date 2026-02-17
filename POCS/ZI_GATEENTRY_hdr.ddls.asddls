@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Gate entry header'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZI_GATEENTRY_hdr
  as select from ztgate_hdr
  composition [0..*] of ZI_GATEENTRY_ITM as _ADV_item
  association [0..1] to ZC_IMAGE_VIEW    as _Image on $projection.ImageId = _Image.ImageId
{
  key zgate      as Zgate,
      plant      as Plant,
      gateindt   as Gateindt,
      gateinout  as Gateinout,
      gateoutdt  as Gateoutdt,
      gateoutout as Gateoutout,
      vehno      as Vehno,
      oname      as Oname,
      drvlicen   as Drvlicen,
      remark     as Remark,
      bill       as Bill,
      tare       as Tare,
      gross      as Gross,
      net        as Net,
      pack       as pack,
      chk        as Chk,
      mblnr      as mblnr,
      gjahr      as gjahr,
      bukrs      as bukrs,
      
      // Expose a constant for image association
      cast( '001' as abap.numc(3) ) as ImageId,

      _ADV_item,
      _Image
}
