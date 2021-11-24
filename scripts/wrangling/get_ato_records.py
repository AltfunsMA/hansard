import urllib.request
from io import BytesIO
import json
import pandas as pd

full_url = 'https://data.gov.au/data/api/3/action/datastore_search?resource_id=552ab4d8-20a0-46ee-927e-4c0af9da6955'  


with urllib.request.urlopen(full_url) as url:
    s = url.read()

j = json.loads(s)

df = pd.DataFrame(j['result']['records'])


df.to_csv("/data/hansard/coal_data/general_stats/tax_by_industry.csv")
