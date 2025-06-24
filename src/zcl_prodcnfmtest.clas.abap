CLASS zcl_prodcnfmtest DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_prodcnfmtest IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

     " read proposals and corresponding times for given quantity
        READ ENTITIES OF i_productionordconfirmationtp
         ENTITY productionorderconfirmation
         EXECUTE getconfproposal
         FROM VALUE #( (
                ConfirmationGroup = '965'
          ) )
         RESULT DATA(lt_confproposal)
         REPORTED DATA(lt_reported_conf).


  ENDMETHOD.
ENDCLASS.
