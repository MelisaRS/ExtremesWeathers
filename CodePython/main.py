import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, LSTM, Dropout
from tensorflow.keras.callbacks import EarlyStopping
from joblib import dump

def load_and_clean_data(filepath):
    df = pd.read_csv(filepath)

    # Filtrar los datos para la ciudad de Nueva York
    df_ny = df[df['City'] == 'Albany']  # Cambiar a 'New York' o 'Albany' según corresponda

    # Reemplazar valores -99 por NaN y eliminar filas con NaN
    df_ny['AvgTemperature'] = df_ny['AvgTemperature'].replace(-99, pd.NA)
    df_cleaned = df_ny.dropna(subset=['AvgTemperature'])

    # Convertir columnas a tipo int
    columns_to_convert = ['Month', 'Day', 'Year']
    for column in columns_to_convert:
        df_cleaned[column] = df_cleaned[column].astype('int')

    # Crear columna 'Date'
    df_cleaned['Date'] = pd.to_datetime(df_cleaned[['Year', 'Month', 'Day']])

    # Convertir Fahrenheit a Celsius
    df_cleaned['AvgTemperature_Celsius'] = (df_cleaned['AvgTemperature'] - 32) * 5.0 / 9.0
    df_cleaned = df_cleaned.drop(columns=['AvgTemperature'])

    print(f"Datos limpiados para Nueva York: {df_cleaned.shape[0]} filas.")
    
    return df_cleaned

def plot_training_history(history):
    plt.figure(figsize=(10, 6))
    plt.plot(history.history['loss'], label='Pérdida de Entrenamiento')
    plt.plot(history.history['val_loss'], label='Pérdida de Validación')
    plt.title('Historial de Entrenamiento')
    plt.xlabel('Épocas')
    plt.ylabel('Pérdida')
    plt.legend()
    plt.grid(True)
    plt.show()

def prepare_lstm_data(data, sequence_length=30, prediction_length=365):
    scaler = MinMaxScaler(feature_range=(0, 1))
    scaled_data = scaler.fit_transform(data.values.reshape(-1, 1))

    X, y = [], []
    for i in range(sequence_length, len(scaled_data) - prediction_length):
        X.append(scaled_data[i-sequence_length:i, 0])
        y.append(scaled_data[i:i+prediction_length, 0])  # Predecir los próximos 365 días

    X = np.array(X)
    y = np.array(y)
    return X, y, scaler

def build_and_train_lstm(X_train, y_train, epochs=50, batch_size=256):
    model = Sequential([
        LSTM(50, return_sequences=True, input_shape=(X_train.shape[1], 1)),
        Dropout(0.2),
        LSTM(50, return_sequences=False),
        Dropout(0.2),
        Dense(365)  # Ahora predice un bloque de 365 días
    ])

    model.compile(optimizer='adam', loss='mean_squared_error', metrics=['mae', 'mse'])

    early_stopping = EarlyStopping(monitor='val_loss', patience=5, restore_best_weights=True)

    history = model.fit(
        X_train, y_train, epochs=epochs, batch_size=batch_size,
        validation_split=0.2, callbacks=[early_stopping], verbose=1
    )
    return model, history

def plot_predictions(y_test, predicted, scaler, df_cleaned, sequence_length=30):
    # Desescalar
    y_test = scaler.inverse_transform(y_test)  
    predicted = scaler.inverse_transform(predicted)  

    # Ajustar las fechas a las dimensiones de y_test
    dates = df_cleaned['Date'].iloc[sequence_length + len(y_test):sequence_length + len(y_test) + len(predicted)].values

    # Graficar los 365 días completos
    plt.figure(figsize=(10, 6))
    plt.plot(dates, y_test[:, 0], color='blue', label='Temperaturas Reales')
    plt.plot(dates, predicted[:, 0], color='red', label='Temperaturas Predichas')
    plt.title('Temperaturas Reales vs Predicciones para los Próximos 365 Días')
    plt.xlabel('Fecha')
    plt.ylabel('Temperatura (°C)')
    plt.legend()
    plt.grid(True)
    plt.show()

def plot_known_data_predictions(df_cleaned, scaler, model, sequence_length=30, prediction_length=365):
    # Usar los últimos 2 años de datos conocidos (cambiar a la cantidad de días que representa 2 años)
    known_data = df_cleaned['AvgTemperature_Celsius'].values[-(2 * 365 + sequence_length):]
    known_data_scaled = scaler.transform(known_data.reshape(-1, 1))

    # Preparar la entrada para la predicción
    X_known = []
    for i in range(sequence_length, len(known_data_scaled)):
        X_known.append(known_data_scaled[i-sequence_length:i, 0])

    X_known = np.array(X_known)

    # Remodelar para LSTM: (n_samples, sequence_length, 1)
    X_known = np.reshape(X_known, (X_known.shape[0], X_known.shape[1], 1))

    # Realizar la predicción
    predictions_known = model.predict(X_known)

    # Desescalar las predicciones
    predictions_known_rescaled = scaler.inverse_transform(predictions_known)

    # Obtener las fechas correspondientes a las predicciones
    last_known_date = df_cleaned['Date'].iloc[-1]
    prediction_dates = pd.date_range(last_known_date + pd.Timedelta(days=1), periods=prediction_length)

    # Asegurarse de que las predicciones y los valores reales tengan la misma longitud
    real_values = df_cleaned['AvgTemperature_Celsius'].values[-prediction_length:]

    # Graficar las predicciones vs los valores reales
    plt.figure(figsize=(10, 6))
    plt.plot(prediction_dates, real_values, color='blue', label='Temperaturas Reales')
    plt.plot(prediction_dates, predictions_known_rescaled[-prediction_length:, 0], color='red', label='Predicciones realizadas')
    plt.title("Predicciones")
    plt.xlabel('Fecha')
    plt.ylabel('Temperatura (°C)')
    plt.legend()
    plt.grid(True)
    plt.show()

def main():
    filepath = './city_temperature.csv'

    # Cargar y limpiar datos
    df_cleaned = load_and_clean_data(filepath)

    # Preparar datos para LSTM
    temperatures = df_cleaned['AvgTemperature_Celsius']
    X, y, scaler = prepare_lstm_data(temperatures, sequence_length=30, prediction_length=365)

    # Dividir datos en entrenamiento y prueba
    split = int(len(X) * 0.8)
    X_train, X_test = X[:split], X[split:]
    y_train, y_test = y[:split], y[split:]

    # Remodelar para LSTM
    X_train = np.reshape(X_train, (X_train.shape[0], X_train.shape[1], 1))
    X_test = np.reshape(X_test, (X_test.shape[0], X_test.shape[1], 1))

    # Entrenar modelo
    model, history = build_and_train_lstm(X_train, y_train, epochs=50, batch_size=64)

    # Mostrar historial de entrenamiento
    plot_training_history(history)

    # Realizar predicciones para los próximos 365 días
    predictions = model.predict(X_test)

    # Graficar resultados
    plot_predictions(y_test, predictions, scaler, df_cleaned)

    # Predicción para los próximos 365 días usando los datos de los últimos 2 años
    last_sequence = X_test[-1]  # Última secuencia de datos de prueba
    future_temperatures = model.predict(last_sequence.reshape(1, -1, 1))

    # Desescalar las predicciones
    future_temperatures_rescaled = scaler.inverse_transform(future_temperatures.reshape(-1, 1))

    # Graficar las predicciones del próximo año
    plt.figure(figsize=(10, 6))
    plt.plot(future_temperatures_rescaled, color='green', label='Predicciones del próximo año')
    plt.title('Predicción de Temperaturas para el Próximo Año')
    plt.xlabel('Día')
    plt.ylabel('Temperatura (°C)')
    plt.legend()
    plt.grid(True)
    plt.show()

    # Graficar predicciones de 2 años vs datos reales
    plot_known_data_predictions(df_cleaned, scaler, model)

    # Guardar el modelo y el scaler
    dump(model, 'model_lstm_temperature_365_days.h5')
    dump(scaler, 'scaler_temperature.pkl')

if __name__ == "__main__":
    main()
