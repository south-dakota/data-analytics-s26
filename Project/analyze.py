import pandas as pd
from sklearn.preprocessing import MultiLabelBinarizer

extremes = pd.read_csv('extremes.csv')
print(extremes.head()) # does it work

extremes['tagsList'] = (extremes['tags'].fillna('').astype(str).str.split(';').apply(lambda x: [t.strip() for t in x if t.strip()]))
mlb = MultiLabelBinarizer()
betterTags = mlb.fit_transform(extremes['tagsList'])
tagsDF = pd.DataFrame(betterTags, columns=mlb.classes_,index=extremes.index)
extremes = pd.concat([extremes, tagsDF], axis=1)
extremes = extremes.drop(columns=['tags', 'tagsList'])

print(extremes)