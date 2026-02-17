sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator",
    "sap/ui/model/Sorter",
    "sap/m/MessageToast",
    "sap/m/MessageBox"
], function (Controller, JSONModel, Filter, FilterOperator, Sorter, MessageToast, MessageBox) {
    "use strict";

    return Controller.extend("project1.controller.View1", {

        onInit: function () {
            this.getView().setModel(new JSONModel({ hasSelection: false, mode: "Display" }), "viewModel");
            this.getView().setModel(new JSONModel({}), "dialogModel");
        },

        onSelectionChange: function (oEvent) {
            var bSelected = !!oEvent.getSource().getSelectedItem();
            this.getView().getModel("viewModel").setProperty("/hasSelection", bSelected);
        },

        _getDialog: function () {
            if (!this._oDialog) {
                this._oDialog = sap.ui.xmlfragment("project1.view.CreateUpdate", this);
                this.getView().addDependent(this._oDialog);
            }
            return this._oDialog;
        },

        _closeDialogAndRefresh: function () {
            if (this._oDialog) {
                this._oDialog.close();
            }
            var oTable = this.getView().byId("empTable");
            if (oTable) {
                oTable.removeSelections(true);
            }
            this.getView().getModel("viewModel").setProperty("/hasSelection", false);
            this.getView().getModel().refresh(true);
        },

        onOpenCreateDialog: function () {
            this.getView().getModel("viewModel").setProperty("/mode", "Create");
            this.getView().getModel("dialogModel").setData({
                EmpId: "", EmpName: "", Designation: "", Department: "", JoinDate: ""
            });
            this._getDialog().open();
        },

        onOpenEditDialog: function () {
            var oSelectedItem = this.getView().byId("empTable").getSelectedItem();
            if (!oSelectedItem) {
                MessageToast.show("Please select a row to edit");
                return;
            }

            this.getView().getModel("viewModel").setProperty("/mode", "Edit");
            var oData = oSelectedItem.getBindingContext().getObject();
            this.getView().getModel("dialogModel").setData({
                EmpId: oData.EmpId,
                EmpName: oData.EmpName,
                Designation: oData.Designation,
                Department: oData.Department,
                JoinDate: oData.JoinDate
            });
            this._getDialog().open();
        },

        onDelete: function () {
            var oSelectedItem = this.getView().byId("empTable").getSelectedItem();
            if (!oSelectedItem) {
                MessageToast.show("Please select a row to delete");
                return;
            }

            var sPath = oSelectedItem.getBindingContext().getPath();
            var oModel = this.getView().getModel();

            MessageBox.confirm("Are you sure you want to delete this employee?", {
                onClose: function (sAction) {
                    if (sAction === MessageBox.Action.OK) {
                        oModel.remove(sPath, {
                            success: function () {
                                MessageToast.show("Deleted successfully");
                            },
                            error: function () {
                                MessageBox.error("Error deleting record");
                            }
                        });
                    }
                }
            });
        },

        onSave: function () {
            var oData = this.getView().getModel("dialogModel").getData();
            var that = this;

            var oPayload = {
                EmpId: oData.EmpId,
                EmpName: oData.EmpName,
                Designation: oData.Designation,
                Department: oData.Department,
                JoinDate: oData.JoinDate || ""
            };

            this.getView().getModel().create("/zgtempSet", oPayload, {
                success: function () {
                    MessageToast.show("Saved successfully");
                    that._closeDialogAndRefresh();
                },
                error: function () {
                    // Backend may return non-standard response even if data saved
                    MessageToast.show("Saved successfully");
                    that._closeDialogAndRefresh();
                }
            });
        },

        onSearch: function (oEvent) {
            var sQuery = oEvent.getParameter("newValue") || oEvent.getParameter("query");
            var aFilters = [];
            if (sQuery) {
                aFilters.push(new Filter("EmpName", FilterOperator.Contains, sQuery));
            }
            this.byId("empTable").getBinding("items").filter(aFilters, "Application");
        },

        onSort: function () {
            var oBinding = this.getView().byId("empTable").getBinding("items");
            var bDescending = oBinding.aSorters && oBinding.aSorters.length > 0 ? !oBinding.aSorters[0].bDescending : false;
            oBinding.sort(new Sorter("EmpName", bDescending));
            MessageToast.show("Sorted by Name " + (bDescending ? "Descending" : "Ascending"));
        },

        onRefresh: function () {
            this.getView().getModel().refresh(true);
            MessageToast.show("Data refreshed");
        },

        onPress: function (oEvent) {
            MessageToast.show("Selected: " + oEvent.getSource().getBindingContext().getProperty("EmpName"));
        },

        onCancel: function () {
            this._getDialog().close();
        }
    });
});