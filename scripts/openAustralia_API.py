from openaustralia import OpenAustralia

oa = OpenAustralia("GU3fvvE5b9exBjwQNRDBSyhu")
search = oa.get_hansard(" coal ", page = 250)
search['rows'].pop(0)
