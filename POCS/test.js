sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "mickey/models/model"
], function (Controller, model) {
    "use strict";

    return Controller.extend("mickey.controller.Main", {
        anotherFx: function () {

            var oView = this.getView();
            var oInput = oView.byId("Idinp");
            var sValue = oInput.getValue();
            alert(sValue);
        },
        anu: 100,
        onInit: function () {

            var oEmpModel = model.createEmployeeModel("models/mockdata/sample.json");
            var oEmpModel2 = model.createEmployeeModel("models/mockdata/sample2.json");


            // Step3 : Set Model to Core
            sap.ui.getCore().setModel(oEmpModel);


            // Step4 : Set another Model to View
            this.getView().setModel(oEmpModel2, "emp2");


            var oResourceModel = model.createResourceModel();
            sap.ui.getCore().setModel(oResourceModel, "i18n");
        },


        onChangeData: function () {
            var oModel = sap.ui.getCore().getModel();

            oModel.setProperty("/empStr/firstName", "Ramesh");
        },

        onRowSelect: function (OEvent) {
            // var oSelectedRow = OEvent.getParameter("rowContext");
            // var oCoreModel = sap.ui.getCore().getModel();
            // var sEmpId = oCoreModel.getProperty("empId", oSelectedRow);
            // alert("Selected Employee ID is : " + sEmpId);

            var oSelectedRow = OEvent.getParameter("rowContext").getPath();

            var oSimpleForm = this.getView().byId("SimpleForm");

            oSimpleForm.bindElement(oSelectedRow);

        },

        convertName: function (sName) {
            console.log("convertName input for firstName:", sName);
            if (!sName) {
                return "";
            }
            return sName.toUpperCase();
        },

        onFlipData: function () {

            var oView = this.getView();
            var oEmp2Model = oView.getModel("emp2");
            var oCoreModel = sap.ui.getCore().getModel();

            if (oView.getModel() === oEmp2Model) {
                oView.setModel(oCoreModel);
            } else {
                oView.setModel(oEmp2Model);
            }

        }

    });
});
