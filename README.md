---
title: "Analise exploratoria inicial"
author: "Julio Trecenti"
date: "2015-07-02"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Analise exploratoria dos dados}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


```r
# dados e api
library(statjobs)

# hadley
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)

# text mining
library(tm)
```

```
## Loading required package: NLP
## 
## Attaching package: 'NLP'
## 
## The following object is masked from 'package:ggplot2':
## 
##     annotate
```

```r
library(textcat)
library(RWeka)
library(wordcloud)
```

```
## Loading required package: RColorBrewer
```

## Analisando evolucao da Doris e do Guilherme no grupo


```r
posts %>%
  filter(from_name %in% c('Doris S M Fontes', 
                          'Guilherme Carrara Neto')) %>%
  mutate(time = ymd_hms(created_time)) %>%
  arrange(time) %>%
  group_by(from_name) %>%
  mutate(um = 1, acu = cumsum(um)) %>%
  ungroup %>%
  ggplot(aes(x = time, y = acu, colour = from_name)) +
  geom_line() +
  theme_bw()
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

## Tratando os textos e analisando com text mining usual

Primeiro, vamos dar uma limpada nos textos. Consideramos só os textos em
portugues, de inicio!

- ignore case
- remove punctuation
- remove stop words
- remove accents
- remove numbers
- remove links and emails


```r
prof <- TC_byte_profiles[names(TC_byte_profiles) %in% c("english", "portuguese")]
posts_messages <- posts %>%
  filter(!is.na(message), type %in% c('status', 'link')) %>%
  mutate(lingua = textcat(message, p = prof)) %>%
  filter(lingua == 'portuguese')

# rm_accent SO FUNCIONA EM LINUX!!!!!!!!!!!!!!!!!!!!
rm_accent <- function(x) gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))
tira_barras <- function(x) gsub('/+|(\\:)+', ' ', x)
tira_emails <- function(x) gsub('[a-zA-Z]+@[a-zA-Z\\-]+\\.[a-zA-Z]+', '', x)
tira_links <- function(x) gsub('http[^ ]+', ' ', x)
# tira_s <- function(x) {gsub('spss', 'spsss', x); gsub('s ', ' ', x)}

banned_words <- c('vaga[^ ]*', 'todo', 'toda', 'trabalh[^ ]*', 
                  'paulo', 'acompanha[^ ]*', 'principal[^ ]*',
                  'profission[^ ]*', 'estatistic[^ ]*',
                  'expiracao', 'janeiro', 'fevereiro', 'marco',
                  'abril', 'maio', 'junho', 'julho', 'agosto',
                  'setembro', 'outubro', 'novembro', 'dezembro',
                  'senior', 'pleno', 'dado', 'data', 'local',
                  'codigo', 'nivel', 'hierarquico', 'rio', 'junior',
                  'estagio', 'quantidade', ' [a-z] ')
banned_words <- paste(banned_words, collapse = '|')
tira_banned <- function(x) gsub(banned_words, ' ', x)

d_tm_raw <- VCorpus(VectorSource(posts_messages$message))
d_tm <- d_tm_raw %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(tira_emails)) %>%
  tm_map(content_transformer(tira_links)) %>%
  tm_map(content_transformer(tira_barras)) %>%
  tm_map(removeWords, stopwords('pt-br')) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = TRUE) %>%
  tm_map(content_transformer(rm_accent)) %>%
  tm_map(content_transformer(tira_links)) %>%
  tm_map(removeWords, unique(rm_accent(stopwords('pt-br')))) %>%
  tm_map(content_transformer(tira_banned)) %>%
  # tm_map(content_transformer(tira_s)) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)
```

### Term document matrix, usando 1-gram e tfidf weighting, sem stemming


```r
ctrl <- list(weighting = function(x) weightTfIdf(x, normalize = FALSE))
dtm <- DocumentTermMatrix(d_tm, control = ctrl)
dtm <- removeSparseTerms(dtm, 0.95)

# como padrao dtm é uma matriz esparsa, boa pra guardar dados mas ruim pra explorar
# por sorte a base de dados é pequena e da pra transformar em data.frame
d_dtm <- data.frame(as.matrix(dtm)) %>%
  add_rownames() %>%
  tbl_df()

# pegando as 10 palavras "mais informativas" de cada documento
palavras <- d_dtm %>%
  mutate(rowname = as.numeric(rowname)) %>%
  gather(key, val, -rowname) %>%
  arrange(desc(val)) %>%
  group_by(rowname) %>%
  slice(1:10) %>%
  ungroup %>%
  filter(val > 0)

wordcloud(palavras$key, 
          max.words = 100, 
          colors = brewer.pal(8, 'Dark2'))
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : marketing could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : desenvolvimento could not be fit on page. It will not be
## plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : logistica could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : business could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : inscricoes could not be fit on page. It will not be
## plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : campanhas could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : programa could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : coordenacao could not be fit on page. It will not be
## plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : processo could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : crm could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : manutencao could not be fit on page. It will not be
## plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : horas could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : risco could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : plano could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : assistencia could not be fit on page. It will not be
## plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : gerar could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : vendas could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : oferece could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : comercial could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : experiencia could not be fit on page. It will not be
## plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : curso could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : planos could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : trainee could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : pretensao could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : regiao could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : dia could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : analise could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : empresa could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : contato could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : auxilio could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : financeiro could not be fit on page. It will not be
## plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : pesquisa could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(palavras$key, max.words = 100, colors =
## brewer.pal(8, : odontologico could not be fit on page. It will not be
## plotted.
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

### Term document matrix, usando 2-gram e tfidf weighting, sem stemming

Nao rodou :(


```r
# BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# ctrl <- list(tokenize = BigramTokenizer)
# dtm <- DocumentTermMatrix(d_tm, control = ctrl)
# dtm <- removeSparseTerms(dtm, 0.95)
# 
# # como padrao dtm é uma matriz esparsa, boa pra guardar dados mas ruim pra explorar
# # por sorte a base de dados é pequena e da pra transformar em data.frame
# d_dtm <- data.frame(as.matrix(dtm)) %>%
#   add_rownames() %>%
#   tbl_df()
# 
# # pegando as 10 palavras "mais informativas" de cada documento
# palavras <- d_dtm %>%
#   mutate(rowname = as.numeric(rowname)) %>%
#   gather(key, val, -rowname) %>%
#   arrange(desc(val)) %>%
#   group_by(rowname) %>%
#   slice(1:10) %>%
#   ungroup %>%
#   filter(val > 0)
# 
# library(wordcloud)
# wordcloud(palavras$key, 
#           max.words = 100, 
#           colors = brewer.pal(8, 'Dark2'))
```

## Analisando as mensagens

Tipos de posts


```r
posts %>%
  count(type, sort = TRUE) %>%
  mutate(` %` = round(n/sum(n) * 100, 2))
```

```
## Source: local data frame [5 x 3]
## 
##     type    n     %
## 1   link 3101 54.19
## 2 status 2501 43.71
## 3  photo  108  1.89
## 4  video   11  0.19
## 5  event    1  0.02
```

Note que são dois tipos de posts principais, status e links. Vamos analisar
separadamente.


```r
status <- posts %>% 
  filter(type %in% 'status')

link <- posts %>% 
  filter(type %in% 'link')
```

Em relação aos status, podemos notar que existe uma grande quantidade de
vagas do site www:.vagas.com.br


```r
status %>%
  mutate(vagas = str_detect(message, 'www\\.vagas\\.com\\.br')) %>%
  count(vagas, sort = TRUE)
```

```
## Source: local data frame [3 x 2]
## 
##   vagas    n
## 1 FALSE 1571
## 2  TRUE  923
## 3    NA    7
```

Geralmente, os posts desse site contém um padrão conhecido.


```r
aux <- status %>%
  mutate(vagas = str_detect(message, 'www\\.vagas\\.com\\.br')) %>%
  filter(vagas) %>%
  slice(1) %>% 
  select(from_name, message) %>% 
  sapply(function(x) {cat(x); cat('\n\n@@@@@@@\n\n')})
```

```
## Guilherme Carrara Neto
## 
## @@@@@@@
## 
## Latin America Strategic Workforce Planning Coordinator (GE - General Electric)
## Nível hierárquico:Supervisão/Coordenação
## Local:São Paulo / SP / BR
## Quantidade de vagas:1
## 
## Role Summary/Purpose 
## We are looking for a Strategic Workforce Planning Coordinator to join a growing team focused on delivering a data-driven consultative experience to internal business clients. Through consultative engagements, the SWP team quantifies and assesses the supply and demand of talent required to successfully inform and execute business strategy.
## 
## Essential Responsibilities 
## The SWFP Coordinator would be engaged in developing and delivering insights for complex workforce planning projects and programs using inputs from a variety of data sources, business leaders and HR Business Partners. This role will drive the projects derived from the WPF analysis. Areas of concentration include consulting, project management, headcount forecasting, scenario planning, and gap analysis.
## Consult with business partners to identify workforce needs, develop hypotheses and execute related quantitative analyses, (e.g. attrition, retirement, and movement)
## Lead the development of a compelling story through data and transform complex data elements into a consumable format for various levels of management.
## Synthesize analyses through activities into comprehensive presentations to foster discussion and communicate key insights.
## Identify and incorporate multiple data elements to support analysis. Facilitate conversations with data owners to ensure proper utilization and management of the data.
## Execute research and analysis and interpret findings to support the identification of solutions that address organizational gaps.
## Lead all the project management activities to ensure the thorough execution of special projects derives from WFP analysis. Continued development of key data sources, tools, templates and processes.
## 
## Qualifications/Requirements 
## Bachelor's degree in economics, Mathematics, Statistics, human resources or a related field.
## 3-7 years of analytical and/or consulting experience.
## strong data management skills
## Demonstrated ability to apply consultative mind-set, problem-solving and critical thinking.
## Team player with strong interpersonal and executive communication.
## Ability to balance multiple priorities simultaneously and meet deadlines as required.
## Proficiency in Microsoft Office Suite (Word, Excel, Access, PowerPoint, Plateu, Datamanagement)
## 
## Desired Characteristics 
## Strategic workforce planning experience
## Statistic and project management experience.
## 
## http://www.vagas.com.br/vagas/v1195884/latin-america-strategic-workforce-planning-coordinator
## 
## @@@@@@@
```

Algumas informações úteis que podem ser extraídas:

- Nível hierárquico
- Nome da vaga (analista, estatístico, etc)
- Atividades
- Requisitos


```r
antes <- c('([^a-zA-Z]|^)formacao\\:?\n?( e pre\\-requisitos\\:)?',
           'academica\\:?\n?',
           '([^a-zA-Z]|^)cursar',
           '([^a-zA-Z]|^)curso\\:?',
           'superior',
           'graduacao')
re <- sprintf('(%s)([^\n]+)', paste(antes, collapse = '|'))
vagas <- status %>%
  mutate(vagas = str_detect(message, 'www\\.vagas\\.com\\.br')) %>%
  filter(vagas) %>%
  mutate(nivel = str_match(message, 'Nível hierárquico\\:([^\n]+)\n')[, 2]) %>%
  mutate(local = str_match(message, 'Local\\:([^\n]+)\n')[, 2]) %>%
  mutate(req = str_match(message, regex('req[^:]+\\:([^|]+)',
                                        ignore_case = TRUE))[, 2]) %>%
  mutate(form = str_match(tolower(tjsp::rm_accent(message)), re)[, 7])
```

Em relação ao nível hierárquico, temos o seguinte resultado:


```r
vagas %>%
  count(nivel, sort = TRUE) %>%
  mutate(` %` = round(n/sum(n) * 100, 2)) %>%
  knitr::kable()
```



|nivel                  |   n|     %|
|:----------------------|---:|-----:|
|Estágio                | 221| 23.94|
|Pleno                  | 212| 22.97|
|Júnior/Trainee         | 199| 21.56|
|Sênior                 | 189| 20.48|
|Supervisão/Coordenação |  43|  4.66|
|Auxiliar/Operacional   |  25|  2.71|
|NA                     |  17|  1.84|
|Gerência               |  13|  1.41|
|Técnico                |   4|  0.43|

Em relação ao local, temos:


```r
vagas %>%
  count(local, sort = TRUE) %>%
  mutate(` %` = round(n/sum(n) * 100, 2)) %>%
  head(10) %>%
  knitr::kable()
```



|local                             |   n|     %|
|:---------------------------------|---:|-----:|
|São Paulo / SP / BR               | 274| 29.69|
|Rio de Janeiro / RJ / BR          | 130| 14.08|
|Curitiba / PR / BR                |  38|  4.12|
|Rio de Janeiro / RJ / BR - Centro |  30|  3.25|
|Belo Horizonte / MG / BR          |  27|  2.93|
|NA                                |  24|  2.60|
|Barueri / SP / BR                 |  20|  2.17|
|Brasil                            |  19|  2.06|
|São Paulo / BR                    |  17|  1.84|
|Campinas / SP / BR                |  16|  1.73|

O grafico abaixo mostra quais os termos mais frequentes quando se fala em formação.


```r
# aux <- vagas %>%
#   sample_n(1) %>% 
#   select(from_name, message, form) %>% 
#   sapply(function(x) {cat(x); cat('\n\n@@@@@@@\n\n')})

cursos <- vagas %>%
  filter(!is.na(form)) %>%
  mutate(matematica = ifelse(str_detect(form, 'matematica'), 'matematica', NA)) %>%
  mutate(contabeis = ifelse(str_detect(form, 'ciencias cont|contab'), 
                            'ciencias contabeis', NA)) %>%
  mutate(engenharia = ifelse(str_detect(form, 'engenharia'), 'engenharia', NA)) %>%
  mutate(economia = ifelse(str_detect(form, 'economia'), 'economia', NA)) %>%
  mutate(financas = ifelse(str_detect(form, 'financas'), 'financas', NA)) %>%
  mutate(atuaria = ifelse(str_detect(form, 'atuari'), 'atuaria', NA)) %>%
  mutate(administracao = ifelse(str_detect(form, 'admin'), 'administracao', NA)) %>%
  mutate(marketing = ifelse(str_detect(form, 'marketing'), 'marketing', NA)) %>%
  mutate(informatica = ifelse(str_detect(form, 'inform|compu'), 'informatica', NA)) %>%
  mutate(estatistica = ifelse(str_detect(form, 'estat'), 'estatistica', NA))
  
cursos %>%
  summarise_each(funs(sum(!is.na(.))), matematica:estatistica) %>%
  gather() %>%
  ggplot(aes(x = reorder(key, X = value), y = value, fill = key)) +
  geom_bar(stat = 'identity') +
  guides(fill = FALSE) +
  theme_bw() +
  xlab('formacao') +
  ylab('quantidade') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

O grafico abaixo mostra quais os termos mais frequentes quando se fala em formação,
apenas quando aparece o nome "estatistica".


```r
cursos %>%
  filter(!is.na(estatistica)) %>%
  summarise_each(funs(sum(!is.na(.))), matematica:estatistica) %>%
  gather() %>%
  mutate(value = value / value[key=='estatistica']) %>%
  filter(key != 'estatistica') %>%
  ggplot(aes(x = reorder(key, X = value), y = value, fill = key)) +
  geom_bar(stat = 'identity') +
  guides(fill = FALSE) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent, breaks = 0:10/10) +
  xlab('formacao') +
  ylab('proporcao') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

TODO

- Pegar atividades
- Pegar requisitos
- Pegar desejáveis
