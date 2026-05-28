import base64
import json
from pathlib import Path

import numpy as np
import pandas as pd
import streamlit as st
from xgboost import XGBClassifier


st.set_page_config(
    page_title="Configuratore bagno intelligente",
    page_icon="🛁",
    layout="wide",
)


BASE_DIR = Path(__file__).resolve().parent
ASSETS_DIR = BASE_DIR / "assets"
ARTIFACTS_DIR = BASE_DIR / "artifacts"
DATASET_PATH = BASE_DIR / "sondaggio_mobili_bagno.csv"
ENTRY_IMAGE_PATH = ASSETS_DIR / "entry_background.png"
COVER_IMAGE_PATH = ASSETS_DIR / "cover_bagno.png"
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
FEATURE_LABELS = {
    "colore": "Colore",
    "dimensione_cm": "Dimensione",
    "type": "Tipologia mobile",
    "lavabo": "Lavabo",
    "numero_cassetti": "Numero cassetti",
    "ante": "Ante",
    "forma_specchiera": "Forma specchiera",
    "illuminazione": "Illuminazione",
}


def apply_global_style():
    st.markdown(
        """
        <style>
            .stApp {
                background:
                    radial-gradient(circle at top left, rgba(239, 228, 212, 0.95), transparent 30%),
                    radial-gradient(circle at bottom right, rgba(195, 178, 160, 0.26), transparent 24%),
                    linear-gradient(135deg, #f6efe5 0%, #eee3d4 100%);
            }
            [data-testid="stHeader"] {
                background: transparent;
            }
            .block-container {
                max-width: 1380px;
                padding-top: 1.2rem;
                padding-bottom: 1rem;
            }
            .hero-card {
                border-radius: 30px;
                overflow: hidden;
                min-height: 360px;
                position: relative;
                background-size: cover;
                background-position: center;
                box-shadow: 0 24px 70px rgba(65, 48, 37, 0.18);
                margin-bottom: 1.2rem;
            }
            .hero-overlay {
                position: absolute;
                inset: 0;
                background:
                    linear-gradient(90deg, rgba(28, 26, 24, 0.78) 0%, rgba(28, 26, 24, 0.54) 32%, rgba(28, 26, 24, 0.16) 62%, rgba(28, 26, 24, 0.02) 100%),
                    linear-gradient(180deg, rgba(241, 230, 214, 0.12), rgba(28, 26, 24, 0.10));
            }
            .hero-content {
                position: relative;
                z-index: 1;
                padding: 3rem 3.2rem;
                width: 46%;
                color: #faf5ef;
            }
            .hero-kicker {
                display: inline-block;
                background: rgba(248, 243, 237, 0.18);
                border: 1px solid rgba(255, 255, 255, 0.22);
                border-radius: 999px;
                padding: 0.42rem 0.85rem;
                font-size: 0.76rem;
                letter-spacing: 0.08em;
                font-weight: 800;
                margin-bottom: 1rem;
            }
            .hero-title {
                font-size: 3.45rem;
                line-height: 0.96;
                font-weight: 900;
                margin-bottom: 0.9rem;
            }
            .hero-subtitle {
                font-size: 1.08rem;
                line-height: 1.5;
                color: rgba(248, 243, 237, 0.93);
                max-width: 560px;
            }
            .panel {
                background: rgba(255, 250, 244, 0.92);
                border: 1px solid rgba(83, 68, 58, 0.10);
                border-radius: 24px;
                padding: 1.2rem 1.25rem;
                box-shadow: 0 18px 48px rgba(70, 54, 44, 0.08);
                height: 100%;
            }
            .section-title {
                font-size: 1.45rem;
                font-weight: 800;
                color: #40342c;
                margin-bottom: 0.25rem;
            }
            .section-subtitle {
                color: #6d5b4f;
                font-size: 0.98rem;
                margin-bottom: 1rem;
            }
            .soft-badge {
                display: inline-block;
                background: #3f5a51;
                color: #f8f3ed;
                border-radius: 999px;
                padding: 0.28rem 0.68rem;
                font-size: 0.74rem;
                font-weight: 800;
                letter-spacing: 0.03em;
            }
            .recommend-box {
                background: linear-gradient(135deg, rgba(232, 241, 236, 0.96), rgba(246, 251, 248, 0.96));
                border: 1px solid rgba(83, 126, 95, 0.22);
                border-radius: 18px;
                padding: 0.95rem 1rem;
                margin-bottom: 0.85rem;
            }
            .meta-box {
                background: rgba(255, 248, 241, 0.96);
                border: 1px solid rgba(83, 68, 58, 0.10);
                border-radius: 18px;
                padding: 0.9rem 1rem;
                margin-bottom: 0.85rem;
            }
            .meta-title {
                font-size: 0.76rem;
                text-transform: uppercase;
                letter-spacing: 0.06em;
                color: #8a7668;
                margin-bottom: 0.3rem;
            }
            .meta-value {
                font-size: 1.05rem;
                font-weight: 800;
                color: #40342c;
            }
            .entry-shell {
                min-height: calc(100vh - 110px);
                display: flex;
                align-items: center;
                justify-content: center;
            }
            .entry-card {
                width: min(1180px, 100%);
                min-height: 620px;
                border-radius: 34px;
                overflow: hidden;
                position: relative;
                background: #ffffff;
                background-size: cover;
                background-position: center;
                box-shadow: 0 28px 80px rgba(65, 48, 37, 0.14);
                border: 1px solid rgba(83, 68, 58, 0.08);
            }
            .entry-overlay {
                position: absolute;
                inset: 0;
                background:
                    linear-gradient(90deg, rgba(255,255,255,0.94) 0%, rgba(255,255,255,0.90) 36%, rgba(255,255,255,0.52) 62%, rgba(255,255,255,0.20) 100%);
            }
            .entry-content {
                position: relative;
                z-index: 1;
                width: 48%;
                padding: 4rem 3.4rem;
            }
            .entry-title {
                font-size: 3.4rem;
                line-height: 0.96;
                font-weight: 900;
                color: #40342c;
                margin-bottom: 1rem;
            }
            .entry-subtitle {
                font-size: 1.05rem;
                line-height: 1.55;
                color: #6d5b4f;
                margin-bottom: 1.8rem;
                max-width: 520px;
            }
            .role-card {
                background: rgba(255, 250, 244, 0.94);
                border: 1px solid rgba(83, 68, 58, 0.10);
                border-radius: 22px;
                padding: 1.1rem 1.15rem;
                margin-bottom: 0.95rem;
            }
            .role-card h4 {
                margin: 0 0 0.35rem 0;
                color: #40342c;
                font-size: 1.06rem;
            }
            .role-card p {
                margin: 0;
                color: #7a695d;
                font-size: 0.94rem;
                line-height: 1.45;
            }
            div[data-testid="stDataFrame"] {
                border-radius: 16px;
                overflow: hidden;
            }
        </style>
        """,
        unsafe_allow_html=True,
    )


def file_to_base64(path):
    return base64.b64encode(path.read_bytes()).decode("utf-8")


@st.cache_data
def load_data():
    if not DATASET_PATH.exists():
        raise FileNotFoundError(f"Dataset non trovato: {DATASET_PATH}")

    data = pd.read_csv(DATASET_PATH)
    if "DAY-Time" in data.columns:
        data = data.drop(columns="DAY-Time")

    data[TARGET] = np.where(data[TARGET] == "Y", 1, 0)
    for feature in FEATURES:
        data[feature] = data[feature].astype(str)

    return data


def build_category_levels(X_raw):
    return {
        feature: sorted(X_raw[feature].dropna().astype(str).unique().tolist())
        for feature in FEATURES
    }


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


def save_model_artifacts(model, category_levels, training_columns):
    ARTIFACTS_DIR.mkdir(parents=True, exist_ok=True)
    model.save_model(MODEL_PATH)
    metadata = {
        "features": FEATURES,
        "training_columns": training_columns,
        "category_levels": category_levels,
        "model_params": {
            "objective": "binary:logistic",
            "eval_metric": "logloss",
            "random_state": 42,
            "n_estimators": 10,
            "max_depth": 3,
            "learning_rate": 0.2,
        },
    }
    METADATA_PATH.write_text(json.dumps(metadata, indent=2), encoding="utf-8")


def train_recommender_assets(dataframe):
    X_raw = dataframe[FEATURES].copy()
    y = dataframe[TARGET].copy()
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
    save_model_artifacts(model, category_levels, training_columns)

    return model, X_raw, category_levels, training_columns, "trained_now"


@st.cache_resource
def load_or_train_recommender(dataframe):
    X_raw = dataframe[FEATURES].copy()

    if MODEL_PATH.exists() and METADATA_PATH.exists():
        metadata = json.loads(METADATA_PATH.read_text(encoding="utf-8"))
        model = XGBClassifier()
        model.load_model(MODEL_PATH)
        return (
            model,
            X_raw,
            metadata["category_levels"],
            metadata["training_columns"],
            "loaded_from_disk",
        )

    return train_recommender_assets(dataframe)


def predict_like_probability(df, model, category_levels, training_columns):
    encoded = stable_encode(df.copy(), category_levels, training_columns)
    return model.predict_proba(encoded)[:, 1]


def filter_compatible_rows(X_raw, partial_config):
    filtered = X_raw.copy()
    for column, value in partial_config.items():
        filtered = filtered[filtered[column] == str(value)]
    return filtered


def rank_options(X_raw, partial_config, next_feature, model, category_levels, training_columns):
    results = []
    possible_values = sorted(X_raw[next_feature].dropna().astype(str).unique())

    for value in possible_values:
        temp_config = partial_config.copy()
        temp_config[next_feature] = value
        compatible_rows = filter_compatible_rows(X_raw, temp_config)

        if len(compatible_rows) == 0:
            avg_prob = np.nan
        else:
            probs = predict_like_probability(
                compatible_rows,
                model,
                category_levels,
                training_columns,
            )
            avg_prob = float(probs.mean())

        results.append(
            {
                "value": value,
                "probabilita_like": avg_prob,
                "n_compatibili": int(len(compatible_rows)),
            }
        )

    ranking = pd.DataFrame(results)
    ranking["sort_key"] = ranking["probabilita_like"].fillna(-1)
    ranking = ranking.sort_values(
        by=["sort_key", "n_compatibili", "value"],
        ascending=[False, False, True],
    ).drop(columns="sort_key")

    return ranking.reset_index(drop=True)


def reset_dependent_config(changed_feature):
    if "config" not in st.session_state:
        st.session_state.config = {}

    cut_index = FEATURES.index(changed_feature)
    for feature in FEATURES[cut_index:]:
        st.session_state.config.pop(feature, None)


def initialize_config_state(X_raw):
    if "config" not in st.session_state:
        st.session_state.config = {}

    if "selected_colore" not in st.session_state:
        st.session_state.selected_colore = sorted(X_raw["colore"].unique())[0]
    if "selected_dimensione" not in st.session_state:
        st.session_state.selected_dimensione = sorted(X_raw["dimensione_cm"].unique())[0]


def sync_initial_config():
    new_color = str(st.session_state.selected_colore)
    new_size = str(st.session_state.selected_dimensione)

    if st.session_state.config.get("colore") != new_color:
        reset_dependent_config("colore")
    st.session_state.config["colore"] = new_color

    if st.session_state.config.get("dimensione_cm") != new_size:
        reset_dependent_config("dimensione_cm")
    st.session_state.config["dimensione_cm"] = new_size


def enter_configurator(role):
    st.session_state.user_role = role
    st.session_state.config = {}
    st.rerun()


def reset_to_entry():
    st.session_state.user_role = None
    st.session_state.config = {}
    st.rerun()


def render_entry_screen():
    background_path = ENTRY_IMAGE_PATH if ENTRY_IMAGE_PATH.exists() else None
    if background_path is None and COVER_IMAGE_PATH.exists():
        background_path = COVER_IMAGE_PATH

    background_style = "background-color:#ffffff;"
    if background_path is not None:
        bg_b64 = file_to_base64(background_path)
        background_style = f"background-image:url('data:image/png;base64,{bg_b64}');"

    st.markdown(
        f"""
        <div class="entry-shell">
            <div class="entry-card" style="{background_style}">
                <div class="entry-overlay"></div>
                <div class="entry-content">
                    <div class="soft-badge">Ingresso</div>
                    <div class="entry-title">Bagno intelligente,<br>dal dato alla scelta</div>
                    <div class="entry-subtitle">
                        Scegli il profilo di accesso e avvia il configuratore guidato.
                        Il percorso è lo stesso per tutti, ma l’esito finale viene mostrato in modo diverso
                        per cliente e agente.
                    </div>
                    <div class="role-card">
                        <h4>Cliente</h4>
                        <p>Esperienza pulita e guidata, focalizzata solo sulla configurazione del bagno.</p>
                    </div>
                    <div class="role-card">
                        <h4>Agente</h4>
                        <p>Stesso percorso del cliente, con visibilità anche sulla probabilità finale stimata dal modello.</p>
                    </div>
                </div>
            </div>
        </div>
        """,
        unsafe_allow_html=True,
    )

    col1, col2, col3 = st.columns([0.22, 0.22, 0.56])
    with col1:
        if st.button("Entra come Cliente", use_container_width=True):
            enter_configurator("cliente")
    with col2:
        if st.button("Entra come Agente", use_container_width=True):
            enter_configurator("agente")


def render_header():
    if not COVER_IMAGE_PATH.exists():
        return

    cover_b64 = file_to_base64(COVER_IMAGE_PATH)
    role = st.session_state.get("user_role", "cliente")
    role_label = "Agente" if role == "agente" else "Cliente"

    st.markdown(
        f"""
        <div class="hero-card" style="background-image:url('data:image/png;base64,{cover_b64}');">
            <div class="hero-overlay"></div>
            <div class="hero-content">
                <div class="hero-kicker">XGBOOST · PROFILO {role_label.upper()}</div>
                <div class="hero-title">Configuratore bagno<br>intelligente</div>
                <div class="hero-subtitle">
                    Il sistema parte da colore e dimensione e accompagna nella scelta delle caratteristiche
                    successive, ordinando le opzioni in base al successo atteso.
                </div>
            </div>
        </div>
        """,
        unsafe_allow_html=True,
    )


def render_option_selector(next_feature, ranking):
    feature_label = FEATURE_LABELS[next_feature]
    ordered_values = ranking["value"].tolist()
    recommended_value = ordered_values[0]

    st.markdown(f"### Prossima scelta: {feature_label}")
    st.markdown(
        f"""
        <div class="recommend-box">
            <div class="soft-badge">Consigliata</div>
            <div style="font-size:1.18rem; font-weight:800; color:#40342c; margin-top:0.35rem;">
                {recommended_value}
            </div>
        </div>
        """,
        unsafe_allow_html=True,
    )

    return st.radio(
        f"Scegli {feature_label.lower()}",
        ordered_values,
        index=0,
        label_visibility="collapsed",
    )


def render_app(model, X_raw, category_levels, training_columns, model_source):
    initialize_config_state(X_raw)
    sync_initial_config()

    role = st.session_state.get("user_role", "cliente")

    top_left, top_right = st.columns([0.82, 0.18])
    with top_right:
        if st.button("Cambia profilo", use_container_width=True):
            reset_to_entry()

    left, right = st.columns([1.15, 0.85], gap="large")

    with left:
        st.markdown('<div class="panel">', unsafe_allow_html=True)
        st.markdown('<div class="section-title">Configurazione guidata</div>', unsafe_allow_html=True)
        st.markdown(
            '<div class="section-subtitle">Si parte da colore e dimensione, poi il sistema propone una caratteristica per volta già ordinata dalla più consigliata alla meno consigliata.</div>',
            unsafe_allow_html=True,
        )

        top_col1, top_col2 = st.columns(2)
        with top_col1:
            st.selectbox(
                "Scegli il colore",
                sorted(X_raw["colore"].unique()),
                key="selected_colore",
            )
        with top_col2:
            st.selectbox(
                "Scegli la dimensione",
                sorted(X_raw["dimensione_cm"].unique()),
                key="selected_dimensione",
            )

        sync_initial_config()

        remaining_features = [feature for feature in FEATURES if feature not in st.session_state.config]

        if remaining_features:
            next_feature = remaining_features[0]
            ranking = rank_options(
                X_raw,
                st.session_state.config,
                next_feature,
                model,
                category_levels,
                training_columns,
            )
            chosen_value = render_option_selector(next_feature, ranking)

            if st.button(f"Conferma {FEATURE_LABELS[next_feature]}", use_container_width=True):
                st.session_state.config[next_feature] = str(chosen_value)
                st.rerun()
        else:
            st.success("Configurazione completata.")

        st.markdown('</div>', unsafe_allow_html=True)

    with right:
        st.markdown('<div class="panel">', unsafe_allow_html=True)
        st.markdown('<div class="section-title">Riepilogo</div>', unsafe_allow_html=True)
        st.markdown(
            '<div class="section-subtitle">Le scelte confermate vengono raccolte qui. La probabilità finale viene mostrata solo al profilo agente.</div>',
            unsafe_allow_html=True,
        )

        source_label = "Modello caricato da file" if model_source == "loaded_from_disk" else "Modello addestrato ora"
        profile_label = "Agente" if role == "agente" else "Cliente"
        st.markdown(
            f"""
            <div class="meta-box">
                <div class="meta-title">Profilo attivo</div>
                <div class="meta-value">{profile_label}</div>
            </div>
            <div class="meta-box">
                <div class="meta-title">Motore</div>
                <div class="meta-value">{source_label}</div>
            </div>
            """,
            unsafe_allow_html=True,
        )

        if st.button("Reset configurazione", use_container_width=True):
            st.session_state.config = {
                "colore": str(st.session_state.selected_colore),
                "dimensione_cm": str(st.session_state.selected_dimensione),
            }
            st.rerun()

        config_table = pd.DataFrame(
            [
                {"Caratteristica": FEATURE_LABELS[key], "Scelta": value}
                for key, value in st.session_state.config.items()
            ]
        )
        st.dataframe(config_table, use_container_width=True, hide_index=True, height=250)

        if len(st.session_state.config) == len(FEATURES):
            if role == "agente":
                final_df = pd.DataFrame([st.session_state.config])
                final_prob = predict_like_probability(
                    final_df,
                    model,
                    category_levels,
                    training_columns,
                )[0]
                st.metric("Probabilità stimata di successo", f"{final_prob:.2%}")
            else:
                st.success("Configurazione pronta. Un agente potrà visualizzare anche l’indicatore finale di successo.")
        else:
            st.info("Completa tutti gli step per terminare la configurazione.")

        st.markdown('</div>', unsafe_allow_html=True)


def main():
    apply_global_style()

    try:
        dataframe = load_data()
    except FileNotFoundError as error:
        st.error(str(error))
        st.stop()

    model, X_raw, category_levels, training_columns, model_source = load_or_train_recommender(dataframe)

    if "user_role" not in st.session_state:
        st.session_state.user_role = None

    if st.session_state.user_role is None:
        render_entry_screen()
        return

    render_header()
    render_app(model, X_raw, category_levels, training_columns, model_source)


if __name__ == "__main__":
    main()
