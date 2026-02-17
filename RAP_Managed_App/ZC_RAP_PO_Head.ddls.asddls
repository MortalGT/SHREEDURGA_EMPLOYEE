@EndUserText.label: 'Projection View for PO Header'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_RAP_PO_Head
  provider contract transactional_query
  as projection on ZI_RAP_PO_Head
{
  key PoUuid,
      PoId,
      Org,
      Status,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      LocalLastChangedAt,
      /* Associations */
      _Item : redirected to composition child ZC_RAP_PO_Item
}
