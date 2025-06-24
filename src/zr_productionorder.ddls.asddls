@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Production Order Value Help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZR_PRODUCTIONORDER as select from I_MfgOrderSequence as production
join I_MfgBOOMaterialAssignment as  Op  on Op.BillOfOperationsGroup = production.BillOfOperationsGroup
   {
   key production.ManufacturingOrder,   
   Op.Product ,
   production.BillOfOperationsGroup,
   production.BillOfOperationsVariant
    
}
