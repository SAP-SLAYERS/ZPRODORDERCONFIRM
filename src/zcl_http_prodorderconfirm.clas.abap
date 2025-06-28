class ZCL_HTTP_PRODORDERCONFIRM definition
  public
  create public .

PUBLIC SECTION.

  TYPES: BEGIN OF tt_mfg_order_activites,
           Quantity TYPE p LENGTH 9 DECIMALS 3,
           Unit     TYPE erfme,
           Name     TYPE c LENGTH 40,
           Item     TYPE i,
         END OF tt_mfg_order_activites.


  TYPES: BEGIN OF tt_mfg_order_movements,
           Material          TYPE matnr,
           Description       TYPE maktx,
           Item              TYPE i,
           Quantity          TYPE menge_d,
           Plant             TYPE werks_d,
           StorageLocation   TYPE c LENGTH 4,
           Batch             TYPE charg_d,
           GoodsMovementType TYPE bwart,
           Unit              TYPE erfme,
         END OF tt_mfg_order_movements.

  TYPES: BEGIN OF tt_response,
           Plant              TYPE werks_d,
           ManufacturingOrder TYPE aufnr,
           Operation          TYPE c LENGTH 4,
           Sequence           TYPE plnfolge,
           PostingDate        TYPE C LENGTH 8,
           Confirmation       TYPE co_rueck,
           YieldQuantity      TYPE menge_d,
           ReworkQuantity     TYPE menge_d,
           ShiftDefinition    TYPE c LENGTH 2,
           _GoodsMovements     TYPE TABLE OF tt_mfg_order_movements WITH EMPTY KEY,
           _Activities         TYPE TABLE OF tt_mfg_order_activites WITH EMPTY KEY,
         END OF tt_response.


  INTERFACES if_http_service_extension .
  CLASS-METHODS getCID RETURNING VALUE(cid) TYPE abp_behv_cid.
  CLASS-METHODS postOrder
    IMPORTING
      VALUE(request) TYPE REF TO if_web_http_request
    RETURNING
      VALUE(message) TYPE string .

   CLASS-METHODS validate
    IMPORTING
      VALUE(filled_details) TYPE tt_response
    RETURNING
      VALUE(message) TYPE string.
protected section.
private section.
ENDCLASS.



CLASS ZCL_HTTP_PRODORDERCONFIRM IMPLEMENTATION.


  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.
     CASE request->get_method(  ).
      WHEN CONV string( if_web_http_client=>post ).
        response->set_text( postOrder( request ) ).
    ENDCASE.

  endmethod.

  METHOD postOrder.

    DATA filled_details TYPE tt_response.

    TRY.
        xco_cp_json=>data->from_string( request->get_text( ) )->write_to( REF #( filled_details ) ).

*        message = validate( filled_details ).
*        IF message IS NOT INITIAL.
*          RETURN.
*        ENDIF.

        DATA: mfgorder          TYPE aufnr,
              mfgorderoperation TYPE c LENGTH 4.
        mfgorder = |{ filled_details-manufacturingorder ALPHA = IN }|.
        mfgorderoperation = |{ filled_details-operation ALPHA = IN }|.

        DATA lt_confirmation TYPE TABLE FOR CREATE i_productionordconfirmationtp.
        DATA lt_matldocitm TYPE TABLE FOR CREATE i_productionordconfirmationtp\_prodnordconfmatldocitm.
        FIELD-SYMBOLS <ls_matldocitm> LIKE LINE OF lt_matldocitm.
        DATA lt_target LIKE <ls_matldocitm>-%target.

        " read proposals and corresponding times for given quantity
        READ ENTITIES OF i_productionordconfirmationtp
         ENTITY productionorderconfirmation
         EXECUTE getconfproposal
         FROM VALUE #( (
                ConfirmationGroup = |{ filled_details-confirmation ALPHA = IN }|
                %param-ConfirmationYieldQuantity = 0
          ) )
         RESULT DATA(lt_confproposal)
         REPORTED DATA(lt_reported_conf).

        LOOP AT lt_confproposal ASSIGNING FIELD-SYMBOL(<ls_confproposal>).
          APPEND INITIAL LINE TO lt_confirmation ASSIGNING FIELD-SYMBOL(<ls_confirmation>).
          <ls_confirmation>-%cid = 'Conf' && sy-tabix..
          <ls_confirmation>-%data = CORRESPONDING #( <ls_confproposal>-%param ).
          <ls_confirmation>-PostingDate = filled_details-postingdate.
          <ls_confirmation>-ConfirmationReworkQuantity = filled_details-reworkquantity.
          <ls_confirmation>-ConfirmationYieldQuantity = filled_details-yieldquantity.
*          <ls_confirmation>-%data-ShiftDefinition = filled_details-shiftdefinition .

          LOOP AT filled_details-_activities INTO DATA(act).
            IF act-item = 1.
              <ls_confirmation>-%data-OpConfirmedWorkQuantity1 = act-quantity.
              <ls_confirmation>-%data-OpWorkQuantityUnit1 = act-unit.
            ELSEIF act-item = 2.
              <ls_confirmation>-%data-OpConfirmedWorkQuantity2 = act-quantity.
              <ls_confirmation>-%data-OpWorkQuantityUnit2 = act-unit.
            ELSEIF act-item = 3.
              <ls_confirmation>-%data-OpConfirmedWorkQuantity3 = act-quantity.
              <ls_confirmation>-%data-OpWorkQuantityUnit3 = act-unit.
            ELSEIF act-item = 4.
              <ls_confirmation>-%data-OpConfirmedWorkQuantity4 = act-quantity.
              <ls_confirmation>-%data-OpWorkQuantityUnit4 = act-unit.
            ELSEIF act-item = 5.
              <ls_confirmation>-%data-OpConfirmedWorkQuantity5 = act-quantity.
              <ls_confirmation>-%data-OpWorkQuantityUnit5 = act-unit.
            ELSEIF act-item = 6.
              <ls_confirmation>-%data-OpConfirmedWorkQuantity6 = act-quantity.
              <ls_confirmation>-%data-OpWorkQuantityUnit6 = act-unit.
            ENDIF.
          ENDLOOP.



          " read proposals for corresponding goods movements for proposed quantity
          READ ENTITIES OF i_productionordconfirmationtp
            ENTITY productionorderconfirmation
            EXECUTE getgdsmvtproposal
            FROM VALUE #( ( confirmationgroup               = <ls_confproposal>-confirmationgroup
                           %param-confirmationyieldquantity = <ls_confproposal>-%param-confirmationyieldquantity
                            ) )
            RESULT DATA(lt_gdsmvtproposal)
            REPORTED DATA(lt_reported_gdsmvt).

          CHECK lt_gdsmvtproposal[] IS NOT INITIAL.

          CLEAR lt_target[].
          LOOP AT lt_gdsmvtproposal ASSIGNING FIELD-SYMBOL(<ls_gdsmvtproposal>) WHERE confirmationgroup = <ls_confproposal>-confirmationgroup.

            LOOP AT filled_details-_goodsmovements INTO DATA(filled_details_goodsmovement) WHERE quantity > 0.
              APPEND INITIAL LINE TO lt_target ASSIGNING FIELD-SYMBOL(<ls_target>).
              <ls_target> = CORRESPONDING #( <ls_gdsmvtproposal>-%param ).
              <ls_target>-%cid = 'Item' && sy-tabix.
              <ls_target>-Material = filled_details_goodsmovement-material.
              <ls_target>-StorageLocation = filled_details_goodsmovement-storagelocation.
              <ls_target>-EntryUnit = filled_details_goodsmovement-unit.
              <ls_target>-GoodsmovementType = filled_details_goodsmovement-goodsmovementtype.
              <ls_target>-QuantityInEntryUnit = filled_details_goodsmovement-quantity.
              <ls_target>-Batch = filled_details_goodsmovement-batch.

              IF filled_details_goodsmovement-goodsmovementtype = '101' OR
                 filled_details_goodsmovement-goodsmovementtype = '102'.
                <ls_target>-OrderItem = '1'.
              ELSEIF filled_details_goodsmovement-goodsmovementtype = '261' OR
                     filled_details_goodsmovement-goodsmovementtype = '262' OR
                     filled_details_goodsmovement-goodsmovementtype = '531' OR
                     filled_details_goodsmovement-goodsmovementtype = '532'.
                <ls_target>-GoodsMovementRefDocType = ''.
                <ls_target>-OrderItem = ''.
              ENDIF.


            ENDLOOP.
          ENDLOOP.


          APPEND VALUE #( %cid_ref = <ls_confirmation>-%cid
          %target = lt_target
          confirmationgroup = <ls_confproposal>-confirmationgroup ) TO lt_matldocitm.
        ENDLOOP.

        MODIFY ENTITIES OF i_productionordconfirmationtp
         ENTITY productionorderconfirmation
         CREATE FROM lt_confirmation
         CREATE BY \_prodnordconfmatldocitm FROM lt_matldocitm
         MAPPED DATA(lt_mapped)
         FAILED DATA(lt_failed)
         REPORTED DATA(lt_reported).

        COMMIT ENTITIES.

        IF sy-msgty = 'E' OR ( sy-msgty = 'I' AND sy-msgid = 'RU' AND sy-msgno = '505' ).
          message = |Error during confirmation: { sy-msgid } { sy-msgno } { sy-msgv1 } { sy-msgv2 } { sy-msgv3 } { sy-msgv4 }|.
          RETURN.
        ENDIF.

        SELECT FROM I_MfgOrderConfirmation
          FIELDS MfgOrderConfirmation
          WHERE ManufacturingOrder = @mfgorder
          AND ManufacturingOrderOperation_2 = @mfgorderoperation
          ORDER BY MfgOrderConfirmation DESCENDING
          INTO TABLE @DATA(mfg_order_confirmation).

        IF mfg_order_confirmation IS INITIAL.
          message = |No confirmation found for manufacturing order { mfgorder } and operation { mfgorderoperation }| .
          RETURN.
        ENDIF.

        message = |Confirmation successful for manufacturing order { mfgorder } and operation { mfgorderoperation } with confirmation number { mfg_order_confirmation[ 1 ]-mfgorderconfirmation }|.

      CATCH cx_root INTO DATA(lx_root).
        message = |General Error: { lx_root->get_text( ) }|.
    ENDTRY.

  ENDMETHOD.

  METHOD validate.

    LOOP AT filled_details-_goodsmovements INTO DATA(ls_goodsmovement) WHERE goodsmovementtype NE '101'.

      DATA material TYPE matnr.
      material = |{ ls_goodsmovement-material ALPHA = IN }|.

      SELECT SINGLE FROM I_StockQuantityCurrentValue_2( P_DisplayCurrency = 'INR' ) AS Stock
         FIELDS  SUM( Stock~MatlWrhsStkQtyInMatlBaseUnit ) AS StockQty
         WHERE Stock~ValuationAreaType = '1'
         AND stock~Product = @material
         AND stock~Plant = @ls_goodsmovement-plant
         AND stock~StorageLocation = @ls_goodsmovement-storagelocation
         AND stock~Batch = @ls_goodsmovement-batch
         INTO @DATA(result).

      IF result IS INITIAL.
        message = |Material { material } not found in stock for plant { ls_goodsmovement-plant } and storage location { ls_goodsmovement-storagelocation }|.
        RETURN.
      ELSEIF result < ls_goodsmovement-quantity.
        message = |Insufficient stock for material { material } in plant { ls_goodsmovement-plant } and storage location { ls_goodsmovement-storagelocation }|.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD getCID.
    TRY.
        cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
      CATCH cx_uuid_error.
        ASSERT 1 = 0.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
