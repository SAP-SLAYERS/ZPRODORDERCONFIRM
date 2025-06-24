class ZCL_HTTP_GENERATEPRODDATA definition
  public
  create public .

PUBLIC SECTION.

  INTERFACES if_http_service_extension .
  CLASS-METHODS fetchDetails
    IMPORTING
      VALUE(request) TYPE REF TO if_web_http_request
    RETURNING
      VALUE(message) TYPE string .
protected section.
private section.
ENDCLASS.



CLASS ZCL_HTTP_GENERATEPRODDATA IMPLEMENTATION.


  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.
     CASE request->get_method(  ).
      WHEN CONV string( if_web_http_client=>get ).
        response->set_text( fetchDetails( request ) ).
        response->set_content_type( 'application/json; charset=utf-8' ).
    ENDCASE.

  endmethod.


  METHOD fetchdetails.

    TYPES: BEGIN OF tt_mfg_order_activites,
             Quantity TYPE menge_d,
             Unit     TYPE erfme,
             Name     TYPE c LENGTH 40,
             Item     TYPE i,
           END OF tt_mfg_order_activites.

    TYPES: BEGIN OF tt_mfg_order_movements,
             Material          TYPE matnr,
             Description       TYPE maktx,
             Multiplier        TYPE p LENGTH 10 DECIMALS 6,
             Item              TYPE i,
             MaterialType      TYPE mtart,
             Quantity          TYPE menge_d,
             Plant             TYPE werks_d,
             StorageLocation   TYPE c LENGTH 4,
             Batch             TYPE charg_d,
             GoodsMovementType TYPE bwart,
             Unit              TYPE erfme,
           END OF tt_mfg_order_movements.

    TYPES: BEGIN OF tt_response,
             Product               TYPE matnr,
             ProductDescription    TYPE maktx,
             Plant                 TYPE werks_d,
             ManufacturingOrder    TYPE aufnr,
             Operation             TYPE c LENGTH 4,
             OperationDescription  TYPE ltxa1,
             Sequence              TYPE plnfolge,
             WorkCenter            TYPE arbpl,
             WorkCenterDescription TYPE c LENGTH 40,
             Confirmation          TYPE co_rueck,
             YieldQuantity         TYPE menge_d,
             YieldUnit             TYPE erfme,
             ReworkQuantity        TYPE menge_d,
             ReworkUnit            TYPE erfme,
             GoodsMovements        TYPE TABLE OF tt_mfg_order_movements WITH EMPTY KEY,
             Activities            TYPE TABLE OF tt_mfg_order_activites WITH EMPTY KEY,
           END OF tt_response.

    DATA order TYPE aufnr.
    DATA response_type TYPE tt_response.

    DATA(coming_order) = request->get_header_field( 'Order' ).

    order = |{ coming_order ALPHA = IN }|.

    SELECT SINGLE FROM I_ManufacturingOrder AS a
        INNER JOIN I_ProductDescription_2 AS b ON a~Product = b~Product
        INNER JOIN I_ManufacturingOrderOperation AS c ON a~MfgOrderInternalID = c~MfgOrderInternalID
        INNER JOIN I_workCenter AS d ON c~WorkCenterTypeCode_2 = d~WorkCenterTypeCode
                                      AND c~WorkCenterInternalID = d~WorkCenterInternalID
        INNER JOIN I_WorkCenterText AS e ON d~WorkCenterInternalID = e~WorkCenterInternalID
                                         AND d~WorkCenterTypeCode   = e~WorkCenterTypeCode
        FIELDS b~ProductDescription,a~Product,a~ManufacturingOrder,a~ProductionPlant,e~WorkCenterText,d~WorkCenter,c~ManufacturingOrderOperation_2,
               c~ManufacturingOrderSequence,c~MfgOrderOperationText,c~OperationConfirmation,a~BillOfOperationsGroup
        WHERE a~ManufacturingOrder = @order
        INTO @DATA(mfg_order_basic).


    response_type-product = mfg_order_basic-Product.
    response_type-productdescription = mfg_order_basic-ProductDescription.
    response_type-plant = mfg_order_basic-ProductionPlant.
    response_type-manufacturingorder = mfg_order_basic-ManufacturingOrder.
    response_type-operation = mfg_order_basic-ManufacturingOrderOperation_2.
    response_type-operationdescription = mfg_order_basic-MfgOrderOperationText.
    response_type-sequence = mfg_order_basic-ManufacturingOrderSequence.
    response_type-workcenter = mfg_order_basic-WorkCenter.
    response_type-workcenterdescription = mfg_order_basic-WorkCenterText.
    response_type-confirmation = mfg_order_basic-OperationConfirmation.


*   Get Activies and Yield Quantities

    READ ENTITIES OF i_productionordconfirmationtp
     ENTITY productionorderconfirmation
     EXECUTE getconfproposal
     FROM VALUE #( (
            ConfirmationGroup = response_type-confirmation
            %param-OrderID = |{ response_type-manufacturingorder ALPHA = IN }|
            %param-OrderOperation = response_type-operation
            %param-Sequence = response_type-sequence
      ) )
     RESULT DATA(lt_confproposal)
     REPORTED DATA(lt_reported_conf).

*    SELECT SINGLE FROM I_BillOfOperationsOpBasic WITH PRIVILEGED ACCESS
*      FIELDS OperationReferenceQuantity,StandardWorkQuantity1,StandardWorkQuantityUnit1,CostCtrActivityType1,
*             StandardWorkQuantity2,StandardWorkQuantityUnit2,CostCtrActivityType2,
*             StandardWorkQuantity3,StandardWorkQuantityUnit3,CostCtrActivityType3,
*             StandardWorkQuantity4,StandardWorkQuantityUnit4,CostCtrActivityType4,
*             StandardWorkQuantity5,StandardWorkQuantityUnit5,CostCtrActivityType5,
*             StandardWorkQuantity6,StandardWorkQuantityUnit6,CostCtrActivityType6
*      WHERE BillOfOperationsGroup = @mfg_order_basic-BillOfOperationsGroup
*            AND Plant = @mfg_order_basic-ProductionPlant
*            AND Operation = @response_type-operation
*      INTO @DATA(bill_of_operations_op_basic).
*


    LOOP AT lt_confproposal INTO DATA(conf_proposal).
       response_type-yieldquantity = conf_proposal-%param-ConfirmationYieldQuantity.
       response_type-yieldunit = conf_proposal-%param-ConfirmationUnit.
       response_type-reworkquantity = conf_proposal-%param-ConfirmationReworkQuantity.
       response_type-reworkunit = conf_proposal-%param-ConfirmationUnit.



    ENDLOOP.





    SELECT SINGLE FROM I_ManufacturingOrderItem AS a
        INNER JOIN I_Product as c ON a~Material = c~Product
        INNER JOIN I_productDescription_2 AS b ON a~Material = b~Product
        FIELDS a~ManufacturingOrder, a~Material,  a~StorageLocation, a~Batch, a~ProductionUnit,b~ProductDescription,c~ProductType
        WHERE ManufacturingOrder = @order
        INTO @DATA(mfg_order_item_status).

    DATA(main_item) = VALUE tt_mfg_order_movements(
             Material          = |{ mfg_order_item_status-Material ALPHA = OUT }|
             Description       = mfg_order_item_status-ProductDescription
             Multiplier        = 1
             Item              = 1
             MaterialType      = mfg_order_item_status-ProductType
             Plant             = mfg_order_basic-ProductionPlant
             StorageLocation   = mfg_order_item_status-StorageLocation
             Batch             = mfg_order_item_status-Batch
             quantity          = response_type-yieldquantity + response_type-reworkquantity
             GoodsMovementType = '101'
             Unit              = mfg_order_item_status-ProductionUnit
     ).
    APPEND main_item TO response_type-GoodsMovements.

    SELECT SINGLE FROM i_billofmaterialtp_2
    FIELDS BillOfMaterial, BOMHeaderQuantityInBaseUnit
    WHERE Material = @mfg_order_item_status-Material
      AND Plant = @mfg_order_basic-ProductionPlant
      AND BillOfMaterialVariantUsage = '1'
      INTO @DATA(bill_of_material).

    SELECT FROM I_MfgOrderComponentWithStatus AS a
        INNER JOIN I_Product as c ON a~Material = c~Product
        INNER JOIN I_ProductDescription_2 AS b ON a~Material = b~Product
        FIELDS a~Material, a~Plant, a~StorageLocation, a~Batch, a~GoodsMovementType, a~EntryUnit,b~ProductDescription,c~ProductType
        WHERE ManufacturingOrder = @order
        INTO TABLE @DATA(mfg_order_components).

    LOOP AT mfg_order_components INTO DATA(mfg_order_component).

      SELECT SINGLE FROM i_billofmaterialitembasic
          FIELDS  BillOfMaterialItemQuantity
            WHERE BillOfMaterial = @bill_of_material-BillOfMaterial
            AND BillOfMaterialComponent = @mfg_order_component-Material
            AND ProdOrderIssueLocation = @mfg_order_component-StorageLocation
            INTO @DATA(bom_item).

      DATA multiplier TYPE p LENGTH 10 DECIMALS 6.
      IF bom_item IS INITIAL OR bom_item = 0.
        multiplier = 1.
      ELSE.
        multiplier = ( bom_item / bill_of_material-BOMHeaderQuantityInBaseUnit ).
      ENDIF.

      DATA(mfg_order_movement) = VALUE tt_mfg_order_movements(
          Material          = |{ mfg_order_component-Material ALPHA = OUT }|
          Description       = mfg_order_component-ProductDescription
          multiplier        = multiplier
          Item              = sy-tabix + 1
          MaterialType      = mfg_order_component-ProductType
          Plant             = mfg_order_component-Plant
          StorageLocation   = mfg_order_component-StorageLocation
          Batch             = mfg_order_component-Batch
          GoodsMovementType = mfg_order_component-GoodsMovementType
          quantity          = multiplier * ( response_type-yieldquantity + response_type-reworkquantity )
          Unit              = mfg_order_component-EntryUnit ).

      APPEND mfg_order_movement TO response_type-GoodsMovements.
    ENDLOOP.


    DATA:json TYPE REF TO if_xco_cp_json_data.

    xco_cp_json=>data->from_abap(
      EXPORTING
        ia_abap      = response_type
      RECEIVING
        ro_json_data = json   ).
    json->to_string(
      RECEIVING
        rv_string =   DATA(lv_string) ).


    REPLACE ALL OCCURRENCES OF '"PRODUCTDESCRIPTION"' IN lv_string WITH '"ProductDescription"'.
    REPLACE ALL OCCURRENCES OF '"PRODUCT"' IN lv_string WITH '"Product"'.
    REPLACE ALL OCCURRENCES OF '"MANUFACTURINGORDER"' IN lv_string WITH '"ManufacturingOrder"'.
    REPLACE ALL OCCURRENCES OF '"OPERATIONDESCRIPTION"' IN lv_string WITH '"OperationDescription"'.
    REPLACE ALL OCCURRENCES OF '"OPERATION"' IN lv_string WITH '"Operation"'.
    REPLACE ALL OCCURRENCES OF '"SEQUENCE"' IN lv_string WITH '"Sequence"'.
    REPLACE ALL OCCURRENCES OF '"WORKCENTERDESCRIPTION"' IN lv_string WITH '"WorkCenterDescription"'.
    REPLACE ALL OCCURRENCES OF '"WORKCENTER"' IN lv_string WITH '"WorkCenter"'.
    REPLACE ALL OCCURRENCES OF '"PLANT"' IN lv_string WITH '"Plant"'.
    REPLACE ALL OCCURRENCES OF '"CONFIRMATION"' IN lv_string WITH '"Confirmation"'.
    REPLACE ALL OCCURRENCES OF '"YIELDQUANTITY"' IN lv_string WITH '"YieldQuantity"'.
    REPLACE ALL OCCURRENCES OF '"YIELDUNIT"' IN lv_string WITH '"YieldUnit"'.
    REPLACE ALL OCCURRENCES OF '"REWORKQUANTITY"' IN lv_string WITH '"ReworkQuantity"'.
    REPLACE ALL OCCURRENCES OF '"REWORKUNIT"' IN lv_string WITH '"ReworkUnit"'.

    REPLACE ALL OCCURRENCES OF '"GOODSMOVEMENTS"' IN lv_string WITH '"_GoodsMovements"'.
    REPLACE ALL OCCURRENCES OF '"MATERIAL"' IN lv_string WITH '"Material"'.
    REPLACE ALL OCCURRENCES OF '"DESCRIPTION"' IN lv_string WITH '"Description"'.
    REPLACE ALL OCCURRENCES OF '"QUANTITY"' IN lv_string WITH '"Quantity"'.
    REPLACE ALL OCCURRENCES OF '"STORAGELOCATION"' IN lv_string WITH '"StorageLocation"'.
    REPLACE ALL OCCURRENCES OF '"BATCH"' IN lv_string WITH '"Batch"'.
    REPLACE ALL OCCURRENCES OF '"GOODSMOVEMENTTYPE"' IN lv_string WITH '"GoodsMovementType"'.
    REPLACE ALL OCCURRENCES OF '"UNIT"' IN lv_string WITH '"Unit"'.
    REPLACE ALL OCCURRENCES OF '"MATERIALTYPE"' IN lv_string WITH '"MaterialType"'.
    REPLACE ALL OCCURRENCES OF '"ITEM"' IN lv_string WITH '"Item"'.
    REPLACE ALL OCCURRENCES OF '"MULTIPLIER"' IN lv_string WITH '"Multiplier"'.
    REPLACE ALL OCCURRENCES OF '"ACTIVITIES"' IN lv_string WITH '"_Activities"'.
    REPLACE ALL OCCURRENCES OF '"NAME"' IN lv_string WITH '"Name"'.
    REPLACE ALL OCCURRENCES OF '"UNIT"' IN lv_string WITH '"Unit"'.
    REPLACE ALL OCCURRENCES OF '"QUANTITY"' IN lv_string WITH '"Quantity"'.



    message = lv_string.

  ENDMETHOD.

ENDCLASS.
