@EndUserText.label: 'Projection View for PO Item'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_RAP_PO_Item
  as projection on ZI_RAP_PO_Item
{
  key ItemUuid,
      PoUuid,
      ItemId,
      Material,
      Quantity,
      Unit,
      Price,
      Currency,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      LocalLastChangedAt,
      /* Associations */
      _Header : redirected to parent ZC_RAP_PO_Head
}
