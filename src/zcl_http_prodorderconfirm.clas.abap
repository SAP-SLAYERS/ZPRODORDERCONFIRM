class ZCL_HTTP_PRODORDERCONFIRM definition
  public
  create public .

public section.

  interfaces IF_HTTP_SERVICE_EXTENSION .
  CLASS-METHODS getCID RETURNING VALUE(cid) TYPE abp_behv_cid.
     CLASS-METHODS postOrder
    IMPORTING
      VALUE(request)  TYPE REF TO if_web_http_request
    RETURNING
      VALUE(message)  TYPE STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_HTTP_PRODORDERCONFIRM IMPLEMENTATION.


  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.
     CASE request->get_method(  ).
      WHEN CONV string( if_web_http_client=>post ).
        response->set_text( postOrder( request ) ).
        response->set_content_type( 'application/json; charset=utf-8' ).
    ENDCASE.

  endmethod.

  METHOD postOrder.
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
             Shift                 TYPE c LENGTH 2,
             Plant                 TYPE werks_d,
             ManufacturingOrder    TYPE aufnr,
             Operation             TYPE c LENGTH 4,
             Sequence              TYPE plnfolge,
             Confirmation          TYPE co_rueck,
             GoodsMovements        TYPE TABLE OF tt_mfg_order_movements WITH EMPTY KEY,
             YieldQuantity         TYPE menge_d,
             ReworkQuantity        TYPE menge_d,
           END OF tt_response.

    DATA filled_details TYPE tt_response.

    TRY.

        xco_cp_json=>data->from_string( request->get_text( ) )->write_to( REF #( filled_details ) ).

        DATA lt_confirmation TYPE TABLE FOR CREATE i_productionordconfirmationtp.
        DATA lt_matldocitm TYPE TABLE FOR CREATE i_productionordconfirmationtp\_prodnordconfmatldocitm.
        FIELD-SYMBOLS <ls_matldocitm> LIKE LINE OF lt_matldocitm.
        DATA lt_target LIKE <ls_matldocitm>-%target.



        " read proposals and corresponding times for given quantity
        READ ENTITIES OF i_productionordconfirmationtp
         ENTITY productionorderconfirmation
         EXECUTE getconfproposal
         FROM VALUE #( (
                ConfirmationGroup = filled_details-confirmation
                %param-OrderID = |{ filled_details-manufacturingorder ALPHA = IN }|
                %param-OrderOperation = filled_details-operation
                %param-Sequence = filled_details-sequence
                %param-ConfirmationYieldQuantity = filled_details-yieldquantity
          ) )
         RESULT DATA(lt_confproposal)
         REPORTED DATA(lt_reported_conf).

        LOOP AT lt_confproposal ASSIGNING FIELD-SYMBOL(<ls_confproposal>).

          APPEND INITIAL LINE TO lt_confirmation ASSIGNING FIELD-SYMBOL(<ls_confirmation>).
          <ls_confirmation>-%cid = getcid( ).
          <ls_confirmation>-%data = CORRESPONDING #( <ls_confproposal>-%param ).
          <ls_confirmation>-%data-ConfirmationReworkQuantity = filled_details-reworkquantity.
          <ls_confirmation>-%data-ConfirmationYieldQuantity = filled_details-yieldquantity.


*         Save the activites here




          " read proposals for corresponding goods movements for proposed quantity
          READ ENTITIES OF i_productionordconfirmationtp
          ENTITY productionorderconfirmation
          EXECUTE getgdsmvtproposal
          FROM VALUE #( ( confirmationgroup = <ls_confproposal>-confirmationgroup ) )
          RESULT DATA(lt_gdsmvtproposal)
          REPORTED DATA(lt_reported_gdsmvt).

          CHECK lt_gdsmvtproposal[] IS NOT INITIAL.

          CLEAR lt_target[].
          DATA(loop) = 1.
          LOOP AT lt_gdsmvtproposal ASSIGNING FIELD-SYMBOL(<ls_gdsmvtproposal>) WHERE confirmationgroup = <ls_confproposal>-confirmationgroup.
            APPEND INITIAL LINE TO lt_target ASSIGNING FIELD-SYMBOL(<ls_target>).
            <ls_target> = CORRESPONDING #( <ls_gdsmvtproposal>-%param ).
            <ls_target>-QuantityInEntryUnit = filled_details-goodsmovements[ loop ]-quantity.
            <ls_target>-Batch = filled_details-goodsmovements[ loop ]-batch.
            <ls_target>-%cid = getcid( ).
            loop = loop + 1.
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



      CATCH cx_root INTO DATA(lx_root).
        message = |General Error: { lx_root->get_text( ) }|.
    ENDTRY.

  ENDMETHOD.

  METHOD getCID.
    TRY.
        cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
      CATCH cx_uuid_error.
        ASSERT 1 = 0.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
