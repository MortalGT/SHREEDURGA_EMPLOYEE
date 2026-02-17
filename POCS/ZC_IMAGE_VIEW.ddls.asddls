@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Image View'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZC_IMAGE_VIEW as select from ztt_image {
  key id as ImageId,
  pic_url as PicUrl,
  text as Text
}
