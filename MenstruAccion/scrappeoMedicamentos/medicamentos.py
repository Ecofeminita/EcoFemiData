import requests
import pandas as pd
import time

url = "https://cnpm.msal.gov.ar/api/vademecum"

headers = {
    "Content-Type": "application/json",
    "User-Agent": "Mozilla/5.0",
    "Origin": "https://www.argentina.gob.ar",
    "Referer": "https://www.argentina.gob.ar/"
}

queries = {
    "estradiol_comprimidos": "estradiol",
    "estriol_gel": "estriol",
    "promestriene_crema": "promestriene",
    "prasterona": "prasterona",
    "progesterona_capsulas": "progesterona",
    "tibolona": "tibolona",
    "testosterona_gel": "testosterona",
    "melatonina": "melatonina",
    "isoflavonas": "isoflavonas",
    "oxazepam": "oxazepam",
    "sertralina": "sertralina",
    "venlafaxina": "venlafaxina",
    "paroxetina": "paroxetina",
    "analgesicos_fem": "fem",
    "vitamina": "vitamina",
    "calcio": "calcio",
    "magnesio": "magnesio",
    "hierro": "hierro"
}

all_results = []

for categoria, query in queries.items():
    print(f"Consultando: {query}")
    payload = {"searchdata": query}

    ok = False
    for intento in range(3):
        try:
            response = requests.post(
                url,
                json=payload,
                headers=headers,
                timeout=(20, 60)
            )
            response.raise_for_status()
            data = response.json()

            for item in data:
                item["categoria_busqueda"] = categoria
                item["query"] = query

            all_results.extend(data)
            ok = True
            break

        except Exception as e:
            print(f"  intento {intento + 1} falló en {query}: {e}")
            time.sleep(3)

    if not ok:
        print(f"  no se pudo obtener respuesta para: {query}")

    time.sleep(1)

df = pd.DataFrame(all_results)
print("\nCantidad total de registros:", len(df))
print("\nColumnas disponibles:")
print(df.columns)

df.to_csv("./MenstruAccion/ScrappeoMedicamentos/scrappeoMedicamentos.csv", index=False, encoding="utf-8-sig")
print("\nCSV exportado: scrappeoMedicamentos.csv")