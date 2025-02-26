xdata_dialog : dialog {
  label = "Agregar/Editar Datos Extendidos";
  : column {
    : boxed_column {
      label = "Datos del Objeto";
      
      : row {
        : text { label = "Marca:"; }
        : edit_box { key = "marca"; width = 20; }
      }
      : row {
        : text { label = "Modelo:"; }
        : edit_box { key = "modelo"; width = 20; }
      }
      : row {
        : text { label = "Número de Serie:"; }
        : edit_box { key = "numSerie"; width = 20; }
      }
      : row {
        : text { label = "Ubicación:"; }
        : edit_box { key = "ubicacion"; width = 20; }
      }
    }
    : row {
      : button {
        label = "Aceptar";
        is_default = true;
        key = "ok_button";
      }
      : button {
        label = "Cancelar";
        key = "cancel_button";
      }
    }
  }
}
