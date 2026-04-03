import requests
import pandas as pd

def fetch_and_save():
    try:
        response = requests.get('https://api.aredl.net/v2/api/aredl/levels?exclude_legacy=true')
        response.raise_for_status()
        data = response.json()
        pdata = pd.json_normalize(data)
        pdata = pdata[(pdata['is_edel_pending'] == False) & (pdata['edel_enjoyment'].notna())]
        # actually remove the unwanted columns; ignore missing ones
        pdata = pdata.drop(columns=['publisher_id','points','legacy','level_id','two_player','description','song','is_edel_pending','gddl_tier','nlw_tier'], errors='ignore')
        if 'tags' in pdata.columns:
            def _tags_for_r(x):
                if isinstance(x, list):
                    return ';'.join([str(i) for i in x])
                if isinstance(x, dict):
                    return ';'.join([f"{k}={v}" for k, v in x.items()])
                if pd.isna(x):
                    return x
                return str(x)
            pdata['tags'] = pdata['tags'].apply(_tags_for_r)
        pdata.to_csv('extremes.csv', index=False)
        print(pdata.head())
        print('Saved to extremes.csv')
    except requests.exceptions.RequestException as e:
        print(f'Request error: {e}')

if __name__ == '__main__':
    fetch_and_save()