import json
from pathlib import Path

import numpy as np
import pandas as pd
from xgboost import XGBClassifier


BASE_DIR = Path(__file__).resolve().parent
ARTIFACTS_DIR = BASE_DIR / "artifacts"
DATASET_PATH = BASE_DIR / "sondaggio_mobili_bagno.csv"
MODEL_PATH = ARTIFACTS_DIR / "xgb_bagno_model.json"
METADATA_PATH = ARTIFACTS_DIR / "xgb_bagno_metadata.json"
TARGET = "RISPOSTA"
FEATURES = [
    "colore",
    "dimensione_cm",
    "type",
    "lavabo",
    "numero_cassetti",
    "ante",
    "forma_specchiera",
    "illuminazione",
]


def load_data():
    df = pd.read_csv(DATASET_PATH)
    if "DAY-Time" in df.columns:
        df = df.drop(columns="DAY-Time")
    df[TARGET] = np.where(df[TARGET] == "Y", 1, 0)
    for feature in FEATURES:
        df[feature] = df[feature].astype(str)
    return df


def build_category_levels(X_raw):
    return {feature: sorted(X_raw[feature].dropna().astype(str).unique().tolist()) for feature in FEATURES}


def build_training_columns(category_levels):
    columns = []
    for feature in FEATURES:
        for level in category_levels[feature][1:]:
            columns.append(f"{feature}_{level}")
    return columns


def stable_encode(df, category_levels, training_columns):
    encoded = pd.DataFrame(0, index=df.index, columns=training_columns, dtype=int)
    for feature in FEATURES:
        feature_series = df[feature].astype(str)
        for level in category_levels[feature][1:]:
            encoded.loc[feature_series == level, f"{feature}_{level}"] = 1
    return encoded


def main():
    ARTIFACTS_DIR.mkdir(parents=True, exist_ok=True)

    df = load_data()
    X_raw = df[FEATURES].copy()
    y = df[TARGET].copy()

    category_levels = build_category_levels(X_raw)
    training_columns = build_training_columns(category_levels)
    X_encoded = stable_encode(X_raw, category_levels, training_columns)

    model = XGBClassifier(
        objective="binary:logistic",
        eval_metric="logloss",
        random_state=42,
        n_estimators=10,
        max_depth=3,
        learning_rate=0.2,
    )
    model.fit(X_encoded, y)
    model.save_model(MODEL_PATH)

    metadata = {
        "features": FEATURES,
        "training_columns": training_columns,
        "category_levels": category_levels,
    }
    METADATA_PATH.write_text(json.dumps(metadata, indent=2), encoding="utf-8")

    print(f"Model saved to: {MODEL_PATH}")
    print(f"Metadata saved to: {METADATA_PATH}")


if __name__ == "__main__":
    main()
