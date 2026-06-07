# Smart Bathroom

Repository condiviso per il progetto **Smart Bathroom** relativo al corso SSDA.

Il progetto raccoglie:
- la webapp Streamlit per il configuratore bagno
- gli script R per analisi dati e clustering 
- gli asset grafici e gli artifact 

## Struttura del progetto

- `app2/`: versione corrente della webapp
- `script_analisi/`: script R usati per analisi e risultati
- `sondaggio_mobili_bagno.ODS`: dataset usato dalla webapp

## Webapp

La versione attuale della webapp si trova in:


app2/app_streamlit.py
Per avviarla in locale spostarsi nella cartella utilizzata ed eseguire:
streamlit run app2/app_streamlit.py


Le librerie necessarie sono elencate in:
app2/requirements.txt
Per installarle:
pip install -r app2/requirements.txt


## Analisi in R
La cartella script_analisi/ contiene gli script usati per:

analisi esplorativa
clustering
modello e risultati sui dati del sondaggio.

Il dataset "ss_ide3_sist.xlsx" contiene le serie storiche relative alle vendite mensili, mentre il dataset "sondaggio_mobili_bagno.csv" integra i risultati al sondaggio forniti dall'azienda con una serie di features inserite a mano da noi tramite le foto.
