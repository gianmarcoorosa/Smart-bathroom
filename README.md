# Smart Bathroom

Repository condiviso per il progetto **Smart Bathroom** relativo al corso SSDA.

Il progetto raccoglie:
- la webapp Streamlit per il configuratore bagno
- gli script R per analisi, clustering e simulazioni
- gli asset grafici e gli artifact del modello XGBoost

## Struttura del progetto

- `app2/`: versione corrente della webapp
- `script_analisi/`: script R usati per analisi e risultati
- `sondaggio_mobili_bagno.csv`: dataset usato dalla webapp

## Webapp

La versione attuale della webapp si trova in:

```bash
app2/app_streamlit.py
Per avviarla in locale:

cd "C:\Users\Utente\Documents\New project 3"
streamlit run app2/app_streamlit.py
Se streamlit non viene riconosciuto:

python -m streamlit run app2/app_streamlit.py
Dipendenze Python
Le librerie necessarie sono elencate in:

app2/requirements.txt
Per installarle:

pip install -r app2/requirements.txt
Modello
Gli artifact del modello XGBoost si trovano in:

app2/artifacts/
Le immagini usate dalla webapp si trovano in:

app2/assets/
Analisi in R
La cartella script_analisi/ contiene gli script usati per:

analisi esplorativa
clustering
simulazioni Monte Carlo
risultati e approfondimenti sui dati
Collaborazione
Per aggiornare la copia locale del repository:

git pull
Per caricare modifiche su GitHub:

git add .
git commit -m "Descrizione modifica"
git push
Nota
La cartella di riferimento per la webapp è app2/.
