class ZCL_HTTP_GENERATEPRODDATA definition
  public
  create public .

PUBLIC SECTION.

  TYPES:BEGIN OF tt_mfg_request,
          ManufacturingOrder TYPE aufnr,
          YieldQuantity      TYPE menge_d,
          ReworkQuantity     TYPE menge_d,
          SaleableWaste      TYPE menge_d,
          ShiftDefinition    TYPE c LENGTH 2,
          RBConsumed         TYPE menge_d,
        END OF tt_mfg_request.

  TYPES: BEGIN OF tt_mfg_order_activites,
           Quantity   TYPE p LENGTH 9 DECIMALS 3,
           Unit       TYPE erfme,
           Multiplier TYPE p LENGTH 10 DECIMALS 8,
           Name       TYPE c LENGTH 40,
           Item       TYPE i,
         END OF tt_mfg_order_activites.

  TYPES: BEGIN OF tt_mfg_order_movements,
           Material          TYPE matnr,
           Description       TYPE maktx,
           Multiplier        TYPE p LENGTH 10 DECIMALS 8,
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
           SaleableWaste         TYPE menge_d,
           ShiftDefinition       TYPE c LENGTH 2,
           RBConsumed            TYPE menge_d,
           GoodsMovements        TYPE TABLE OF tt_mfg_order_movements WITH EMPTY KEY,
           Activities            TYPE TABLE OF tt_mfg_order_activites WITH EMPTY KEY,
         END OF tt_response.

  CLASS-DATA response_type TYPE tt_response.

  CLASS-METHODS validate
    IMPORTING
      VALUE(filled_details) TYPE tt_mfg_request
    RETURNING
      VALUE(message)        TYPE string.


  INTERFACES if_http_service_extension .
  CLASS-METHODS fetchDetails
    IMPORTING
      VALUE(request) TYPE REF TO if_web_http_request
    RETURNING
      VALUE(message) TYPE string .

  CLASS-METHODS sendActivity
    IMPORTING
      VALUE(name)     TYPE string
      VALUE(quantity) TYPE p
      VALUE(over_qty) TYPE p
      VALUE(activity) TYPE lstar
      VALUE(unit)     TYPE erfme.
protected section.
private section.
ENDCLASS.



CLASS ZCL_HTTP_GENERATEPRODDATA IMPLEMENTATION.


  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.
     CASE request->get_method(  ).
      WHEN CONV string( if_web_http_client=>post ).
        response->set_text( fetchDetails( request ) ).
        response->set_content_type( 'application/json; charset=utf-8' ).
    ENDCASE.

  endmethod.


  METHOD fetchdetails.


    DATA request_data TYPE tt_mfg_request.

    TRY.
        xco_cp_json=>data->from_string( request->get_text( ) )->write_to( REF #( request_data ) ).
      CATCH cx_root INTO DATA(lx_root).
        message = |General Error: { lx_root->get_text( ) }|.
    ENDTRY.

    message = validate( request_data ).
    IF message IS NOT INITIAL.
      RETURN.
    ENDIF.


    request_data-ManufacturingOrder = |{ request_data-ManufacturingOrder ALPHA = IN }|.

    SELECT SINGLE FROM I_ManufacturingOrder AS a
        INNER JOIN I_ProductDescription_2 AS b ON a~Product = b~Product
        INNER JOIN I_ManufacturingOrderOperation AS c ON a~MfgOrderInternalID = c~MfgOrderInternalID
        INNER JOIN I_workCenter AS d ON c~WorkCenterTypeCode_2 = d~WorkCenterTypeCode
                                      AND c~WorkCenterInternalID = d~WorkCenterInternalID
        INNER JOIN I_WorkCenterText AS e ON d~WorkCenterInternalID = e~WorkCenterInternalID
                                         AND d~WorkCenterTypeCode   = e~WorkCenterTypeCode
        FIELDS b~ProductDescription,a~Product,a~ManufacturingOrder,a~ProductionPlant,e~WorkCenterText,d~WorkCenter,c~ManufacturingOrderOperation_2,
               c~ManufacturingOrderSequence,c~MfgOrderOperationText,c~OperationConfirmation,a~BillOfOperationsGroup
        WHERE a~ManufacturingOrder = @request_data-ManufacturingOrder
        INTO @DATA(mfg_order_basic).


    response_type-product = |{ mfg_order_basic-Product ALPHA = OUT }|.
    response_type-productdescription = mfg_order_basic-ProductDescription.
    response_type-plant = mfg_order_basic-ProductionPlant.
    response_type-manufacturingorder = |{ mfg_order_basic-ManufacturingOrder ALPHA = OUT }|.
    response_type-operation = |{ mfg_order_basic-ManufacturingOrderOperation_2 ALPHA = OUT }|.
    response_type-operationdescription = mfg_order_basic-MfgOrderOperationText.
    response_type-sequence = |{ mfg_order_basic-ManufacturingOrderOperation_2 ALPHA = OUT }|.
    response_type-workcenter = mfg_order_basic-WorkCenter.
    response_type-workcenterdescription = mfg_order_basic-WorkCenterText.
    response_type-confirmation = |{ mfg_order_basic-OperationConfirmation ALPHA = OUT }|.
    response_type-saleablewaste = request_data-SaleableWaste.
    response_type-rbconsumed = request_data-RBConsumed.
    response_type-shiftdefinition = request_data-ShiftDefinition.


*   Get Activities and Yield Quantities

    READ ENTITIES OF i_productionordconfirmationtp
     ENTITY productionorderconfirmation
     EXECUTE getconfproposal
     FROM VALUE #( (
            ConfirmationGroup = mfg_order_basic-OperationConfirmation
            %param-ConfirmationYieldQuantity = request_data-YieldQuantity
            %param-ConfirmationReworkQuantity = request_data-ReworkQuantity
            %param-OrderID =  mfg_order_basic-ManufacturingOrder
            %param-OrderOperation = mfg_order_basic-ManufacturingOrderOperation_2
            %param-Sequence = mfg_order_basic-ManufacturingOrderOperation_2
      ) )
     RESULT DATA(lt_confproposal)
     REPORTED DATA(lt_reported_conf).

    LOOP AT lt_confproposal INTO DATA(conf_proposal).
      response_type-yieldquantity = conf_proposal-%param-ConfirmationYieldQuantity.
      response_type-yieldunit = conf_proposal-%param-ConfirmationUnit.
      response_type-reworkquantity = conf_proposal-%param-ConfirmationReworkQuantity.
      response_type-reworkunit = conf_proposal-%param-ConfirmationUnit.

      SELECT SINGLE FROM I_ProductionOrderOperationTP
         FIELDS OperationReferenceQuantity,WorkCenterStandardWorkQtyUnit1,WorkCenterStandardWorkQty1,CostCtrActivityType1,
                  WorkCenterStandardWorkQtyUnit2,WorkCenterStandardWorkQty2,CostCtrActivityType2,
                 WorkCenterStandardWorkQtyUnit3,WorkCenterStandardWorkQty3,CostCtrActivityType3,
                 WorkCenterStandardWorkQtyUnit4,WorkCenterStandardWorkQty4,CostCtrActivityType4,
                 WorkCenterStandardWorkQtyUnit5,WorkCenterStandardWorkQty5,CostCtrActivityType5,
                 WorkCenterStandardWorkQtyUnit6,WorkCenterStandardWorkQty6,CostCtrActivityType6
            WHERE ProductionOrder = @mfg_order_basic-ManufacturingOrder
              AND ProductionOrderOperation = @mfg_order_basic-ManufacturingOrderOperation_2
              AND Plant = @mfg_order_basic-ProductionPlant
              AND WorkCenter = @response_type-workcenter
              INTO @DATA(bill_of_operations_op_basic).


*      Labor
      sendactivity(
         name = 'Labour'
         quantity = bill_of_operations_op_basic-WorkCenterStandardWorkQty1
         over_qty = bill_of_operations_op_basic-OperationReferenceQuantity
         activity = bill_of_operations_op_basic-CostCtrActivityType1
         unit = bill_of_operations_op_basic-WorkCenterStandardWorkQtyUnit1 ).

*      Power
      sendactivity(
          name = 'Power'
          quantity = bill_of_operations_op_basic-WorkCenterStandardWorkQty2
          over_qty = bill_of_operations_op_basic-OperationReferenceQuantity
          activity = bill_of_operations_op_basic-CostCtrActivityType2
          unit = bill_of_operations_op_basic-WorkCenterStandardWorkQtyUnit2 ).

*      Fuel
      sendactivity(
          name = 'Fuel'
          quantity = bill_of_operations_op_basic-WorkCenterStandardWorkQty3
          over_qty = bill_of_operations_op_basic-OperationReferenceQuantity
          activity = bill_of_operations_op_basic-CostCtrActivityType3
          unit = bill_of_operations_op_basic-WorkCenterStandardWorkQtyUnit3 ).

*      Repair And Maintenance
      sendactivity(
          name = 'Repair And Maint.'
          quantity = bill_of_operations_op_basic-WorkCenterStandardWorkQty4
          over_qty = bill_of_operations_op_basic-OperationReferenceQuantity
          activity = bill_of_operations_op_basic-CostCtrActivityType4
          unit = bill_of_operations_op_basic-WorkCenterStandardWorkQtyUnit4 ).

*      Overheads
      sendactivity(
          name = 'Overheads'
          quantity = bill_of_operations_op_basic-WorkCenterStandardWorkQty5
          over_qty = bill_of_operations_op_basic-OperationReferenceQuantity
          activity = bill_of_operations_op_basic-CostCtrActivityType5
          unit = bill_of_operations_op_basic-WorkCenterStandardWorkQtyUnit5 ).

*      Machine
      sendactivity(
          name = 'Machine'
          quantity = bill_of_operations_op_basic-WorkCenterStandardWorkQty6
          over_qty = bill_of_operations_op_basic-OperationReferenceQuantity
          activity = bill_of_operations_op_basic-CostCtrActivityType6
          unit = bill_of_operations_op_basic-WorkCenterStandardWorkQtyUnit6 ).

    ENDLOOP.

    SELECT SINGLE FROM I_ManufacturingOrderItem AS a
        INNER JOIN I_Product AS c ON a~Material = c~Product
        INNER JOIN I_productDescription_2 AS b ON a~Material = b~Product
        FIELDS a~ManufacturingOrder, a~Material,  a~StorageLocation, a~Batch, a~ProductionUnit,b~ProductDescription,c~ProductType
        WHERE ManufacturingOrder = @mfg_order_basic-ManufacturingOrder
        INTO @DATA(mfg_order_item_status).

    DATA(main_item) = VALUE tt_mfg_order_movements(
             Material          = mfg_order_item_status-Material
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
        INNER JOIN I_Product AS c ON a~Material = c~Product
        INNER JOIN I_ProductDescription_2 AS b ON a~Material = b~Product
        FIELDS a~Material, a~Plant, a~StorageLocation, a~Batch, a~GoodsMovementType, a~EntryUnit,b~ProductDescription,
               c~ProductType,a~BillOfMaterialItemNumber
        WHERE ManufacturingOrder = @mfg_order_basic-ManufacturingOrder
        ORDER BY a~BillOfMaterialItemNumber
        INTO TABLE @DATA(mfg_order_components).

    LOOP AT mfg_order_components INTO DATA(mfg_order_component).

      SELECT SINGLE FROM I_UnitOfMeasure
     FIELDS UnitOfMeasure_E
     WHERE UnitOfMeasure = @mfg_order_component-EntryUnit
     INTO @DATA(unit1).


      IF mfg_order_component-GoodsMovementType = '531' AND mfg_order_component-ProductType = 'ZCOP'.
        DATA(mfg_order_movement) = VALUE tt_mfg_order_movements(
             Material          = mfg_order_component-Material
             Description       = mfg_order_component-ProductDescription
             Item              = lines( response_type-GoodsMovements ) + 1
             MaterialType      = mfg_order_component-ProductType
             Plant             = mfg_order_component-Plant
             StorageLocation   = mfg_order_component-StorageLocation
             GoodsMovementType = mfg_order_component-GoodsMovementType
             quantity          = request_data-reworkquantity
             Unit              = unit1 ).
        APPEND mfg_order_movement TO response_type-GoodsMovements.
        CONTINUE.
      ELSEIF mfg_order_component-GoodsMovementType = '261' AND mfg_order_component-ProductType = 'ZCOP'.
        mfg_order_movement = VALUE tt_mfg_order_movements(
             Material          = mfg_order_component-Material
             Description       = mfg_order_component-ProductDescription
             Item              = lines( response_type-GoodsMovements ) + 1
             MaterialType      = mfg_order_component-ProductType
             Plant             = mfg_order_component-Plant
             StorageLocation   = mfg_order_component-StorageLocation
             GoodsMovementType = mfg_order_component-GoodsMovementType
             quantity          = request_data-saleablewaste
             Unit              = unit1 ).
        APPEND mfg_order_movement TO response_type-GoodsMovements.
        CONTINUE.
      ELSEIF mfg_order_component-GoodsMovementType = '531' AND mfg_order_component-ProductType = 'ZNVM'.
        mfg_order_movement = VALUE tt_mfg_order_movements(
             Material          = mfg_order_component-Material
             Description       = mfg_order_component-ProductDescription
             Item              = lines( response_type-GoodsMovements ) + 1
             MaterialType      = mfg_order_component-ProductType
             Plant             = mfg_order_component-Plant
             StorageLocation   = mfg_order_component-StorageLocation
             GoodsMovementType = mfg_order_component-GoodsMovementType
             quantity          = request_data-rbconsumed
             Unit              = unit1 ).
        APPEND mfg_order_movement TO response_type-GoodsMovements.
        CONTINUE.
      ENDIF.


      SELECT SINGLE FROM i_billofmaterialitembasic
          FIELDS  BillOfMaterialItemQuantity
            WHERE BillOfMaterial = @bill_of_material-BillOfMaterial
            AND BillOfMaterialComponent = @mfg_order_component-Material
            AND ProdOrderIssueLocation = @mfg_order_component-StorageLocation
            AND BillOfMaterialItemNumber = @mfg_order_component-BillOfMaterialItemNumber
            INTO @DATA(bom_item).

      DATA multiplier TYPE p LENGTH 10 DECIMALS 6.
      IF bom_item IS INITIAL OR bom_item = 0.
        multiplier = 1.
      ELSE.
        multiplier = ( bom_item / bill_of_material-BOMHeaderQuantityInBaseUnit ).
      ENDIF.

      DATA itew_quantity TYPE menge_d.
      itew_quantity = multiplier * ( response_type-yieldquantity + response_type-reworkquantity ).

      DATA(today) = cl_abap_context_info=>get_system_date(  ).

      SELECT FROM I_StockQuantityCurrentValue_2( p_displaycurrency = 'INR' ) AS a
           INNER JOIN I_Batch AS b ON a~Batch = b~Batch AND a~Plant = b~Plant AND b~ShelfLifeExpirationDate >= @today
           FIELDS a~MatlWrhsStkQtyInMatlBaseUnit, b~Batch
           WHERE a~Product = @mfg_order_component-Material
             AND a~Plant = @mfg_order_component-Plant
             AND a~StorageLocation = @mfg_order_component-StorageLocation
             AND a~ValuationAreaType = '1'
             AND a~MatlWrhsStkQtyInMatlBaseUnit > 0
             ORDER BY b~ShelfLifeExpirationDate ASCENDING
             INTO TABLE @DATA(stock_with_batch).

      IF stock_with_batch IS INITIAL.
        mfg_order_movement = VALUE tt_mfg_order_movements(
          Material          = mfg_order_component-Material
          Description       = mfg_order_component-ProductDescription
          Multiplier        = multiplier
          Item              = lines( response_type-GoodsMovements ) + 1
          MaterialType      = mfg_order_component-ProductType
          Plant             = mfg_order_component-Plant
          StorageLocation   = mfg_order_component-StorageLocation
          GoodsMovementType = mfg_order_component-GoodsMovementType
          quantity          = itew_quantity
          Unit              = unit1 ).

        APPEND mfg_order_movement TO response_type-GoodsMovements.
        CONTINUE.
      ENDIF.


      LOOP AT stock_with_batch INTO DATA(stock_item).
        IF itew_quantity <= 0.
          EXIT.
        ENDIF.

        mfg_order_movement = VALUE tt_mfg_order_movements(
          Material          = mfg_order_component-Material
          Description       = mfg_order_component-ProductDescription
          Multiplier        = multiplier
          Item              = lines( response_type-GoodsMovements ) + 1
          MaterialType      = mfg_order_component-ProductType
          Plant             = mfg_order_component-Plant
          StorageLocation   = mfg_order_component-StorageLocation
          Batch             = |{ stock_item-Batch ALPHA = OUT }|
          GoodsMovementType = mfg_order_component-GoodsMovementType
          quantity          = COND #(
                                        WHEN itew_quantity >= stock_item-MatlWrhsStkQtyInMatlBaseUnit
                                          THEN stock_item-MatlWrhsStkQtyInMatlBaseUnit
                                        ELSE itew_quantity
                                     )
          Unit              = unit1 ).

        APPEND mfg_order_movement TO response_type-GoodsMovements.
        itew_quantity -= stock_item-MatlWrhsStkQtyInMatlBaseUnit.
      ENDLOOP.
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
    REPLACE ALL OCCURRENCES OF '"SALEABLEWASTE"' IN lv_string WITH '"SaleableWaste"'.
    REPLACE ALL OCCURRENCES OF '"RBCONSUMED"' IN lv_string WITH '"RBConsumed"'.
    REPLACE ALL OCCURRENCES OF '"SHIFTDEFINITION"' IN lv_string WITH '"ShiftDefinition"'.



    message = lv_string.

  ENDMETHOD.

  METHOD validate.

    filled_details-ManufacturingOrder = |{ filled_details-ManufacturingOrder ALPHA = IN }|.

    SELECT SINGLE FROM I_MfgOrderWithStatus
      FIELDS OrderIsReleased,OrderIsDelivered,OrderIsTechnicallyCompleted,OrderIsDeleted
      WHERE ManufacturingOrder = @filled_details-ManufacturingOrder
      INTO @DATA(mfg_order).

    IF mfg_order IS INITIAL.
      message = |Manufacturing Order { filled_details-ManufacturingOrder } does not exist.| .
      RETURN.
    ELSEIF mfg_order-OrderIsReleased IS INITIAL.
      message = |Manufacturing Order { filled_details-ManufacturingOrder } is not released.| .
      RETURN.
    ELSEIF mfg_order-OrderIsDelivered IS NOT INITIAL.
      message = |Manufacturing Order { filled_details-ManufacturingOrder } is already delivered.| .
      RETURN.
    ELSEIF mfg_order-OrderIsTechnicallyCompleted IS NOT INITIAL.
      message = |Manufacturing Order { filled_details-ManufacturingOrder } is technically completed.| .
      RETURN.
    ELSEIF mfg_order-OrderIsDeleted IS NOT INITIAL.
      message = |Manufacturing Order { filled_details-ManufacturingOrder } is deleted.| .
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD sendActivity.
    DATA: op_multiplier TYPE p LENGTH 10 DECIMALS 8,
          qty TYPE p LENGTH 9 DECIMALS 3.
    IF activity IS NOT INITIAL.
            op_multiplier = quantity / over_qty.
            qty = op_multiplier * ( response_type-yieldquantity + response_type-reworkquantity ).

            SELECT SINGLE FROM I_UnitOfMeasure
                FIELDS UnitOfMeasure_E
                WHERE UnitOfMeasure = @unit
                INTO @DATA(unit1).

            DATA(activit) = VALUE tt_mfg_order_activites(
                                    Item       = lines( response_type-Activities ) + 1
                                    Name       = to_upper( name )
                                    Multiplier = op_multiplier
                                    Quantity   = qty
                                    Unit       = unit1
                             ).
            APPEND activit TO response_type-Activities.
        ENDIF.

  ENDMETHOD.
ENDCLASS.
