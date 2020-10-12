from lxml import objectify
import pandas as pd
xml = objectify.parse(open('XMLData.xml'))
root = xml.getroot()
df = pd.DataFrame(columns=('Number', 'String',' Boolean'))
for i in range(0,4):
    obj = root.getchildren()[i].getchildren()
    row = dict(zip(['Number','String','Boolean'],[obj[0].text,obj[1].text,obj[2].text ]))
    row_s = pd.Series(row)
    row_s.name = i
    df = df.append(row_s)
print(df)