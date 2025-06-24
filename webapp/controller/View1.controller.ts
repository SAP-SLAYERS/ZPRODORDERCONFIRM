import Controller from "sap/ui/core/mvc/Controller";
import SearchField from "sap/m/SearchField";
import Label from "sap/m/Label";
import Text from "sap/m/Text";
import UIColumn from "sap/ui/table/Column";
import MColumn from "sap/m/Column";
import ColumnListItem from "sap/m/ColumnListItem";
import TypeString from "sap/ui/model/type/String";
import ValueHelpDialog from "sap/ui/comp/valuehelpdialog/ValueHelpDialog";
import ODataModel from "sap/ui/model/odata/v2/ODataModel";
import Filter from "sap/ui/model/Filter";
import FilterOperator from "sap/ui/model/FilterOperator";
import Token from "sap/m/Token";
import BusyIndicator from "sap/ui/core/BusyIndicator";
import MessageToast from "sap/m/MessageToast";
import JSONModel from "sap/ui/model/json/JSONModel";
import DateFormat from "sap/ui/core/format/DateFormat";

export default class View1 extends Controller {
    private _oBasicSearchField: SearchField | null = null;
    private ProductionOrderVHDialog: ValueHelpDialog | null = null;


    public formModel: JSONModel = new JSONModel({
        Product: "",
        ProductDescription: "",
        Plant: "",
        ManufacturingOrder: "",
        Operation: "",
        OperationDescription: "",
        Sequence: "",
        WorkCenter: "",
        WorkCenterDescription: "",
        Confirmation: "",

    });


    public onInit(): void {
        const oModel = new ODataModel("/sap/opu/odata/sap/ZUI_CAPACITYCATEGORY");
        this.getView()?.setModel(oModel);
        this.getView()?.setModel(this.formModel, "formModel");
        // this.getView()?.setModel(this.formModel);




    }
    public async onProductionOrder(oEvent: any): Promise<void> {
        if (this.ProductionOrderVHDialog) {
            this.ProductionOrderVHDialog.destroy();
            this.ProductionOrderVHDialog = null;
        }

        this._oBasicSearchField = new SearchField({
            search: () => {
                this.ProductionOrderVHDialog?.getFilterBar().search();
            }
        });

        await this.loadFragment({
            name: "zproductionorder.view.fragment.Order"
        }).then((oDialog: any) => {
            this.ProductionOrderVHDialog = oDialog;
            const oView = this.getView();
            oView?.addDependent(oDialog);
            oDialog.setModel(oView?.getModel());

            const oFilterBar = oDialog.getFilterBar();
            oFilterBar.setFilterBarExpanded(false);
            oFilterBar.setBasicSearch(this._oBasicSearchField);

            oDialog.setRangeKeyFields([{
                key: "ManufacturingOrder",
                label: "ManufacturingOrder",
                type: "string",
                typeInstance: new TypeString({}, { maxLength: 12 })
            } as any]);

            oDialog.getTableAsync().then((oTable: any) => {
                oTable.setSelectionMode("Single");

                // (desktop/table)
                if (oTable.bindRows) {
                    oTable.attachRowSelectionChange((oEvent: any) => {
                        const iIndex = oEvent.getParameter("rowIndex");
                        const oContext = oTable.getContextByIndex(iIndex);
                        const sOrder = oContext?.getProperty("ManufacturingOrder");

                        if (sOrder) {
                            const oToken = new Token({
                                key: sOrder,
                                text: sOrder
                            });
                            this.ProductionOrderVHDialog?.setTokens([oToken]);
                        }

                    });

                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Material" }), template: new Text({ text: "{Product}" }) }));
                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Group" }), template: new Text({ text: "{BillOfOperationsGroup}" }) }));
                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Group Counter" }), template: new Text({ text: "{BillOfOperationsVariant}" }) }));
                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Order" }), template: new Text({ text: "{ManufacturingOrder}" }) }));

                    oTable.bindAggregation("rows", {
                        path: "/zc_productionorder"
                    });
                }

                // Handle sap.m.Table (responsive/mobile)
                else if (oTable.bindItems) {
                    oTable.attachSelectionChange((oEvent: any) => {
                        const oItem = oEvent.getParameter("listItem");
                        const oContext = oItem?.getBindingContext();
                        const sOrder = oContext?.getProperty("ManufacturingOrder");

                        if (sOrder) {
                            const oToken = new Token({
                                key: sOrder,
                                text: sOrder
                            });
                            this.ProductionOrderVHDialog?.setTokens([oToken]);
                        }
                    });

                    oTable.addColumn(new MColumn({ header: new Label({ text: "Material" }) }));
                    oTable.addColumn(new MColumn({ header: new Label({ text: "Group" }) }));
                    oTable.addColumn(new MColumn({ header: new Label({ text: "Group Counter" }) }));
                    oTable.addColumn(new MColumn({ header: new Label({ text: "Order" }) }));

                    oTable.bindItems({
                        path: "/zc_productionorder",
                        template: new ColumnListItem({
                            cells: [
                                new Label({ text: "{Product}" }),
                                new Label({ text: "{BillOfOperationsGroup}" }),
                                new Label({ text: "{BillOfOperationsVariant}" }),
                                new Label({ text: "{ManufacturingOrder}" })
                            ]
                        })
                    });
                }

                oDialog.update();
            });

            oDialog.open();
        });
    }


    public onVHSearch(oEvent: any): void {
        const sSearchQuery = this._oBasicSearchField?.getValue()?.trim() || "";
        const aSelectionSet = oEvent.getParameter("selectionSet");

        const aFilters = aSelectionSet.reduce((aResult: any[], oControl: any) => {
            const sValue = oControl.getValue()?.trim();
            if (sValue) {
                aResult.push(new Filter({
                    path: oControl.getName(),
                    operator: FilterOperator.Contains,
                    value1: sValue
                }));
            }
            return aResult;
        }, []);


        if (sSearchQuery) {
            aFilters.push(new Filter({
                filters: [
                    new Filter("ManufacturingOrder", FilterOperator.Contains, sSearchQuery),

                ],
                and: false
            }));
        }

        console.log("Search Query:", sSearchQuery);
        console.log("Final Filters:", aFilters);

        const oFilter = aFilters.length > 0 ? new Filter({ filters: aFilters, and: true }) : undefined;

        this._filterOperationVHTable(oFilter);
    }

    private _filterOperationVHTable(oFilter: Filter | undefined): void {
        const oDialog = this.ProductionOrderVHDialog;

        oDialog?.getTableAsync().then((oTable: any) => {
            const sAggregation = oTable.bindRows ? "rows" : "items";
            const oBinding = oTable.getBinding(sAggregation);

            if (oFilter) {
                oBinding.filter(oFilter);
            } else {
                oBinding.filter([]); // Clear filters if nothing is provided
            }

            oDialog.update();
        });
    }

    public onVHOkPress(oEvent: any): void {
        const aTokens = oEvent.getParameter("tokens");
        const selectedOrder = aTokens[0]?.getText();

        if (selectedOrder) {
            (this.byId("_IDGenInput1") as any).setValue(selectedOrder);
        }

        this.ProductionOrderVHDialog?.close();
    }

    public onVHCancelPress(): void {
        this.ProductionOrderVHDialog?.close();
    }

    public onVHAfterClose(): void {
        this.ProductionOrderVHDialog?.destroy();
        this.ProductionOrderVHDialog = null;
        this._oBasicSearchField = null;
    }
    public async onOperation(oEvent: any): Promise<void> {
        if (this.ProductionOrderVHDialog) {
            this.ProductionOrderVHDialog.destroy();
            this.ProductionOrderVHDialog = null;
        }

        this._oBasicSearchField = new SearchField({
            search: () => {
                this.ProductionOrderVHDialog?.getFilterBar().search();
            }
        });

        await this.loadFragment({
            name: "zproductionorder.view.fragment.Operation"
        }).then((oDialog: any) => {
            this.ProductionOrderVHDialog = oDialog;
            const oView = this.getView();
            oView?.addDependent(oDialog);
            oDialog.setModel(oView?.getModel());

            const oFilterBar = oDialog.getFilterBar();
            oFilterBar.setFilterBarExpanded(false);
            oFilterBar.setBasicSearch(this._oBasicSearchField);

            oDialog.setRangeKeyFields([{
                key: "Plant",
                label: "plant",
                type: "string",
                typeInstance: new TypeString({}, { maxLength: 12 })
            } as any]);

            oDialog.getTableAsync().then((oTable: any) => {
                oTable.setSelectionMode("Single");

                // (desktop/table)
                if (oTable.bindRows) {
                    oTable.attachRowSelectionChange((oEvent: any) => {
                        const iIndex = oEvent.getParameter("rowIndex");
                        const oContext = oTable.getContextByIndex(iIndex);
                        const opOrder = oContext?.getProperty("ManufacturingOrder");

                        if (opOrder) {
                            const oToken = new Token({
                                key: opOrder,
                                text: opOrder
                            });
                            this.ProductionOrderVHDialog?.setTokens([oToken]);
                        }

                    });
                }

                if (oTable.bindRows) {

                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Activity" }), template: new Text({ text: "{ManufacturingOrder}" }) }));
                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Sequence" }), template: new Text({ text: "{ManufacturingOrderSequence}" }) }));
                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Operation Quantity" }), template: new Text({ text: "{ConfirmationTotalQuantity}" }) }));
                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Operation unit" }), template: new Text({ text: "{ConfirmationUnit}" }) }));
                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Operation Short Text" }), template: new Text({ text: "{MfgOrderOperationText}" }) }));
                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Confirmed Yield" }), template: new Text({ text: "{ConfirmationYieldQuantity}" }) }));
                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Confirmed Scrap" }), template: new Text({ text: "{ConfirmationScrapQuantity}" }) }));
                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Confirmed Rework" }), template: new Text({ text: "{ConfirmationReworkQuantity}" }) }));
                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Plant" }), template: new Text({ text: "{Plant}" }) }));



                    oTable.bindAggregation("rows", {
                        path: "/zc_operation"
                    });
                }
                // Handle sap.m.Table (responsive/mobile)
                else if (oTable.bindItems) {
                    oTable.attachSelectionChange((oEvent: any) => {
                        const oItem = oEvent.getParameter("listItem");
                        const oContext = oItem?.getBindingContext();
                        const oOrder = oContext?.getProperty("ManufacturingOrder");

                        if (oOrder) {
                            const oToken = new Token({
                                key: oOrder,
                                text: oOrder
                            });
                            this.ProductionOrderVHDialog?.setTokens([oToken]);
                        }
                    });
                }

                if (oTable.bindItems) {

                    oTable.addColumn(new MColumn({ header: new Label({ text: "Activity" }) }));
                    oTable.addColumn(new MColumn({ header: new Label({ text: "Sequence" }) }));
                    oTable.addColumn(new MColumn({ header: new Label({ text: "Operation Quantity" }) }));
                    oTable.addColumn(new MColumn({ header: new Label({ text: "Operation unit" }) }));
                    oTable.addColumn(new MColumn({ header: new Label({ text: "Operation Short Text" }) }));
                    oTable.addColumn(new MColumn({ header: new Label({ text: "Confirmed Yield" }) }));
                    oTable.addColumn(new MColumn({ header: new Label({ text: "Confirmed Scrap" }) }));
                    oTable.addColumn(new MColumn({ header: new Label({ text: "Confirmed Rework" }) }));
                    oTable.addColumn(new MColumn({ header: new Label({ text: "Plant" }) }));


                    oTable.bindItems({
                        path: "/zc_operation",
                        template: new ColumnListItem({
                            cells: [

                                new Label({ text: "{ManufacturingOrder}" }),
                                new Label({ text: "{ManufacturingOrderSequence}" }),
                                new Label({ text: "{ConfirmationTotalQuantity}" }),
                                new Label({ text: "{ConfirmationUnit}" }),
                                new Label({ text: "{MfgOrderOperationText}" }),
                                new Label({ text: "{ConfirmationYieldQuantity}" }),
                                new Label({ text: "{ConfirmationScrapQuantity}" }),
                                new Label({ text: "{ConfirmationReworkQuantity}" })
                            ]
                        })
                    });
                }

                oDialog.update();
            });

            oDialog.open();
        });
    }

    public onOPSearch(oEvent: any): void {
        const sSearchQuery = this._oBasicSearchField?.getValue()?.trim() || "";
        const aSelectionSet = oEvent.getParameter("selectionSet");

        const aFilters = aSelectionSet.reduce((aResult: any[], oControl: any) => {
            const sValue = oControl.getValue()?.trim();
            if (sValue) {
                aResult.push(new Filter({
                    path: oControl.getName(),
                    operator: FilterOperator.Contains,
                    value1: sValue
                }));
            }
            return aResult;
        }, []);


        if (sSearchQuery) {
            aFilters.push(new Filter({
                filters: [
                    new Filter("ManufacturingOrder ", FilterOperator.Contains, sSearchQuery),

                ],
                and: false
            }));
        }

        console.log("Search Query:", sSearchQuery);
        console.log("Final Filters:", aFilters);

        const oFilter = aFilters.length > 0 ? new Filter({ filters: aFilters, and: true }) : undefined;

        this._filterProductionOrderVHTable(oFilter);
    }

    private _filterProductionOrderVHTable(oFilter: Filter | undefined): void {
        const oDialog = this.ProductionOrderVHDialog;

        oDialog?.getTableAsync().then((oTable: any) => {
            const sAggregation = oTable.bindRows ? "rows" : "items";
            const oBinding = oTable.getBinding(sAggregation);

            if (oFilter) {
                oBinding.filter(oFilter);
            } else {
                oBinding.filter([]); // Clear filters if nothing is provided
            }

            oDialog.update();
        });
    }

    public onOPOkPress(oEvent: any): void {
        const aTokens = oEvent.getParameter("tokens");
        const selectedOrder = aTokens[0]?.getText();

        if (selectedOrder) {
            (this.byId("_IDGenInput2") as any).setValue(selectedOrder);
        }

        this.ProductionOrderVHDialog?.close();
    }

    public onOPCancelPress(): void {
        this.ProductionOrderVHDialog?.close();
    }

    public onOPAfterClose(): void {
        this.ProductionOrderVHDialog?.destroy();
        this.ProductionOrderVHDialog = null;
        this._oBasicSearchField = null;
    }
    public async onUnitOfMeasure(oEvent: any): Promise<void> {
        if (this.ProductionOrderVHDialog) {
            this.ProductionOrderVHDialog.destroy();
            this.ProductionOrderVHDialog = null;
        }

        this._oBasicSearchField = new SearchField({
            search: () => {
                this.ProductionOrderVHDialog?.getFilterBar().search();
            }
        });

        await this.loadFragment({
            name: "zproductionorder.view.fragment.Unit"
        }).then((oDialog: any) => {
            this.ProductionOrderVHDialog = oDialog;
            const oView = this.getView();
            oView?.addDependent(oDialog);
            oDialog.setModel(oView?.getModel());

            const oFilterBar = oDialog.getFilterBar();
            oFilterBar.setFilterBarExpanded(false);
            oFilterBar.setBasicSearch(this._oBasicSearchField);

            oDialog.setRangeKeyFields([{
                key: "UnitOfMeasure",
                label: "UnitOfMeasure",
                type: "string",
                typeInstance: new TypeString({}, { maxLength: 12 })
            } as any]);

            oDialog.getTableAsync().then((oTable: any) => {
                oTable.setSelectionMode("Single");

                // (desktop/table)
                if (oTable.bindRows) {
                    oTable.attachRowSelectionChange((oEvent: any) => {
                        const iIndex = oEvent.getParameter("rowIndex");
                        const oContext = oTable.getContextByIndex(iIndex);
                        const opOrder = oContext?.getProperty("UnitOfMeasure");

                        if (opOrder) {
                            const oToken = new Token({
                                key: opOrder,
                                text: opOrder
                            });
                            this.ProductionOrderVHDialog?.setTokens([oToken]);
                        }

                    });
                }

                if (oTable.bindRows) {

                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Commercial" }), template: new Text({ text: "{UnitOfMeasure}" }) }));
                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Internal UoM" }), template: new Text({ text: "{UnitOfMeasureSAPCode}" }) }));
                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Meas. Unit Text" }), template: new Text({ text: "{UnitOfMeasureLongName}" }) }));
                    oTable.addColumn(new UIColumn({ label: new Label({ text: "Dimension Text" }), template: new Text({ text: "{UnitOfMeasureDimensionName}" }) }));


                    oTable.bindAggregation("rows", {
                        path: "/ZC_UNIT"
                    });
                }
                // Handle sap.m.Table (responsive/mobile)
                else if (oTable.bindItems) {
                    oTable.attachSelectionChange((oEvent: any) => {
                        const oItem = oEvent.getParameter("listItem");
                        const oContext = oItem?.getBindingContext();
                        const oOrder = oContext?.getProperty("Plant");

                        if (oOrder) {
                            const oToken = new Token({
                                key: oOrder,
                                text: oOrder
                            });
                            this.ProductionOrderVHDialog?.setTokens([oToken]);
                        }
                    });
                }

                if (oTable.bindItems) {

                    oTable.addColumn(new MColumn({ header: new Label({ text: "Commercial" }) }));
                    oTable.addColumn(new MColumn({ header: new Label({ text: "Internal UoM" }) }));
                    oTable.addColumn(new MColumn({ header: new Label({ text: "Meas. Unit Text" }) }));
                    oTable.addColumn(new MColumn({ header: new Label({ text: "Dimension Text:" }) }));


                    oTable.bindItems({
                        path: "/ZC_UNIT",
                        template: new ColumnListItem({
                            cells: [

                                new Label({ text: "{UnitOfMeasure}" }),
                                new Label({ text: "{UnitOfMeasureSAPCode}" }),
                                new Label({ text: "{UnitOfMeasureLongName}" }),
                                new Label({ text: "{UnitOfMeasureDimensionName}" }),

                            ]
                        })
                    });
                }

                oDialog.update();
            });

            oDialog.open();
        });
    }

    public onUnitVHSearch(oEvent: any): void {
        const sSearchQuery = this._oBasicSearchField?.getValue()?.trim() || "";
        const aSelectionSet = oEvent.getParameter("selectionSet");

        const aFilters = aSelectionSet.reduce((aResult: any[], oControl: any) => {
            const sValue = oControl.getValue()?.trim();
            if (sValue) {
                aResult.push(new Filter({
                    path: oControl.getName(),
                    operator: FilterOperator.Contains,
                    value1: sValue
                }));
            }
            return aResult;
        }, []);


        if (sSearchQuery) {
            aFilters.push(new Filter({
                filters: [
                    new Filter("UnitOfMeasure", FilterOperator.Contains, sSearchQuery),

                ],
                and: false
            }));
        }

        console.log("Search Query:", sSearchQuery);
        console.log("Final Filters:", aFilters);

        const oFilter = aFilters.length > 0 ? new Filter({ filters: aFilters, and: true }) : undefined;

        this._filterProductionOrderVHTable(oFilter);
    }

    private _filterUnitVHTable(oFilter: Filter | undefined): void {
        const oDialog = this.ProductionOrderVHDialog;

        oDialog?.getTableAsync().then((oTable: any) => {
            const sAggregation = oTable.bindRows ? "rows" : "items";
            const oBinding = oTable.getBinding(sAggregation);

            if (oFilter) {
                oBinding.filter(oFilter);
            } else {
                oBinding.filter([]); // Clear filters if nothing is provided
            }

            oDialog.update();
        });
    }

    public onUnitVHOkPress(oEvent: any): void {
        const aTokens = oEvent.getParameter("tokens");
        const selectedOrder = aTokens[0]?.getText();

        if (selectedOrder) {
            (this.byId("_IDGenInput15") as any).setValue(selectedOrder);
        }

        this.ProductionOrderVHDialog?.close();
    }

    public onUnitVHCancelPress(): void {
        this.ProductionOrderVHDialog?.close();
    }

    public onUnitVHAfterClose(): void {
        this.ProductionOrderVHDialog?.destroy();
        this.ProductionOrderVHDialog = null;
        this._oBasicSearchField = null;
    }




    public onQtyChange() {
        let Yield = Number(this.formModel.getProperty("/YieldQuantity") ),
            Rework = Number(this.formModel.getProperty("/ReworkQuantity") );
        let lines = [...this.formModel.getProperty("/_GoodsMovements")];
        for (let index = 0; index < lines.length; index++) {
            const element = lines[index];
            if (element.GoodsMovementType === '101') {
                lines[index].Quantity = (Yield + Rework);
            }
            else if (element.GoodsMovementType === '261' && element.MaterialType !== 'ZCOP') {
                lines[index].Quantity = Number(Number(element.Multiplier) * (Yield + Rework)).toFixed(3)
            }
            else if (element.GoodsMovementType === '531' && element.MaterialType === 'ZCOP') {
                lines[index].Quantity = Number(Rework).toFixed(3)
            }
        }
        this.formModel.setProperty("/_GoodsMovements", lines);
    }

    public saleableQtyChange(oEvent:any){
        const qty = oEvent.getParameter("value");
        let lines = [...this.formModel.getProperty("/_GoodsMovements")];
        for (let index = 0; index < lines.length; index++) {
            const element = lines[index];
            if (element.GoodsMovementType === '531' && element.MaterialType === 'ZNVM') {
                lines[index].Quantity = Number(qty).toFixed(3)
            }
        }
        this.formModel.setProperty("/_GoodsMovements", lines);
    }

    public RBQtyChange(oEvent:any){
        const qty = oEvent.getParameter("value");
        let lines = [...this.formModel.getProperty("/_GoodsMovements")];
        for (let index = 0; index < lines.length; index++) {
            const element = lines[index];
            if (element.GoodsMovementType === '261' && element.MaterialType === 'ZCOP') {
                lines[index].Quantity = Number(qty).toFixed(3)
            }
        }
        this.formModel.setProperty("/_GoodsMovements", lines);
    }



    public onGenerate() {

        const sValue = this.formModel.getProperty("/ManufacturingOrder");
        const that = this;

        if (!sValue) {
            MessageToast.show("Please enter a Manufacturing Order");
            return;
        }

        BusyIndicator.show();

        $.ajax({
            url: "/sap/bc/http/sap/ZHTTP_GENERATEPRODDATA",
            method: "GET",
            headers: {
                "Order": sValue
            },
            contentType: "application/json",
            success: function (response) {
                if (response) {
                    that.formModel.setData({
                        ...that.formModel.getData(),
                        ...response,
                        ManufacturingOrder: sValue
                    });

                    var oNow = new Date();

                    // Format date as "yyyy-MM-ddTHH:mm:ss" (for DatePicker)
                    var oDateFormatter = DateFormat.getDateInstance({ pattern: "yyyy-MM-ddTHH:mm:ss" });
                    var sFormattedDate = oDateFormatter.format(oNow);
                    that.formModel.setProperty("/PostingDate",sFormattedDate)
            

                    MessageToast.show("Fields auto-filled successfully");
                } else {
                    MessageToast.show("No data found for the entered order");
                }
                BusyIndicator.hide();
            },
            error: function (error) {
                MessageToast.show("Update failed: " + (error.responseText || "Unknown error"));
                BusyIndicator.hide();
            }
        });
    }

    public onClickPost() {

        const data = this.formModel.getData();

        $.ajax({
            url: "/sap/bc/http/sap/ZHTTP_PRODORDERCONFIRM",
            method: "POST",
            data: JSON.stringify(data),
            contentType: "application/json",
            success: function (response) {
                if (response) {
                    debugger

                    MessageToast.show("Fields auto-filled successfully");
                } else {
                    MessageToast.show("No data found for the entered order");
                }
                BusyIndicator.hide();
            },
            error: function (error) {
                MessageToast.show("Update failed: " + (error.responseText || "Unknown error"));
                BusyIndicator.hide();
            }
        });
    }


}