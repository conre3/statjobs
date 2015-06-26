# statjobs

Pesquisa Statjobs usando Graph API do Facebook

## Instalação

Este pacote não está disponível no CRAN. Para instalá-lo, utilize o pacote `devtools`, fazendo

```
devtools::install_github('conre3/statjobs')
```

## Pequeno manual de utilização

- Pegue um **access token** temporário na página https://developers.facebook.com/tools/explorer (um código enorme)
- rode

```
at <- 'codigo enorme'
lista <- statjobs::baixa_posts(at)
dados <- statjobs::arruma_posts(lista)
```

O pacote já vem com uma base baixada em 2015-06-25. Para carregar, rode

```
data(posts, package = 'statjobs')
```

Divirta-se ;)
