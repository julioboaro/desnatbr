# Junta bases Gaul1 (Estados) e Gaul2 (Municípios) da FAO (usada em desastres)
# Base de municípios vem separada. Junção de municípios com estados evita
# problemas posteriores de ambiguidade de municípios com mesmo nome
# Júlio Boaro em 15-05-2022

# Fontes:
# https://data.amerigeoss.org/nl/dataset/regional-boundaries-gaul-global-admin-1
# https://data.amerigeoss.org/nl/dataset/regional-boundaries-gaul-global-admin-2

# Estados do Brasil (apenas para conferência)
gaul1 <- read_csv("data/gaul_adm1.csv") %>% 
  select(gaul_adm1, adm1_name) %>%  # Seleciona só o que interessa
  filter(gaul_adm1 >= 665,
         gaul_adm1 <= 691)

# Muncípios do Brasil
gaul2 <- read_csv("data/gaul_adm2.csv") %>% 
  select(gaul_adm2, adm2_name) %>%  # Seleciona só o que interessa
  filter(gaul_adm2 >= 6333,
         gaul_adm2 <= 11842)

# Adiciona códigos Gaul1 (Estados)
gaul2 <- gaul2 %>% 
  mutate(gaul_adm1 = case_when(
    gaul_adm2 >= 6333 & gaul_adm2 <= 6354 ~ 665,  # Acre
    gaul_adm2 >= 6355 & gaul_adm2 <= 6455 ~ 666,  # Alagoas
    gaul_adm2 >= 6456 & gaul_adm2 <= 6471 ~ 667,  # Amapá
    gaul_adm2 >= 6472 & gaul_adm2 <= 6533 ~ 668,  # Amazonas
    gaul_adm2 >= 6534 & gaul_adm2 <= 6946 ~ 669,  # Bahia
    gaul_adm2 >= 6947 & gaul_adm2 <= 7130 ~ 670,  # Ceará
    gaul_adm2 == 7131 ~ 671,  # Distrito Federal
    gaul_adm2 >= 7132 & gaul_adm2 <= 7208 ~ 672,  # Espírito Santo
    gaul_adm2 >= 7209 & gaul_adm2 <= 7450 ~ 673,  # Goiás
    gaul_adm2 >= 7451 & gaul_adm2 <= 7667 ~ 674,  # Maranhão
    gaul_adm2 >= 7745 & gaul_adm2 <= 7870 ~ 675,  # Mato Grosso
    gaul_adm2 >= 7668 & gaul_adm2 <= 7744 ~ 676,  # Mato Grosso do Sul
    gaul_adm2 >= 7871 & gaul_adm2 <= 8723 ~ 677,  # Minas Gerais
    gaul_adm2 >= 8724 & gaul_adm2 <= 8866 ~ 678,  # Pará
    gaul_adm2 >= 8867 & gaul_adm2 <= 9089 ~ 679,  # Paraíba
    gaul_adm2 >= 9090 & gaul_adm2 <= 9488 ~ 680,  # Paraná
    gaul_adm2 >= 9489 & gaul_adm2 <= 9673 ~ 681,  # Pernambuco
    gaul_adm2 >= 9674 & gaul_adm2 <= 9894 ~ 682,  # Piauí
    gaul_adm2 >= 9895 & gaul_adm2 <= 9985 ~ 683,  # Rio de Janeiro
    gaul_adm2 >= 9986 & gaul_adm2 <= 10151 ~ 684,  # Rio Grande do Norte
    gaul_adm2 >= 10152 & gaul_adm2 <= 10618 ~ 685,  # Rio Grande do Sul
    gaul_adm2 >= 10618 & gaul_adm2 <= 10670 ~ 686,  # Rondônia
    gaul_adm2 >= 10671 & gaul_adm2 <= 10685 ~ 687,  # Roraima
    gaul_adm2 >= 10686 & gaul_adm2 <= 10978 ~ 688,  # Santa Catarina
    gaul_adm2 >= 10979 & gaul_adm2 <= 11623 ~ 689,  # São Paulo
    gaul_adm2 >= 11624 & gaul_adm2 <= 11699 ~ 690,  # Sergipe
    gaul_adm2 >= 11700 & gaul_adm2 <= 11838 ~ 691  # Tocantins
  ))

# Fazendo uma revisão de baixo para cima, por contagem de Estados, notamos que
# o município de Canavieira que conta como em Sergipe na verdade fica na Bahia.
# Deve haver outros erros como este que podem ser corrigidos no bloco a seguir
# Correções:
gaul2 <- gaul2 %>% 
  mutate(gaul_adm1 = ifelse(gaul_adm2 == 11633, 669, gaul_adm1))

# Correção de nomes
gaul2 <- gaul2 %>% 
  mutate(
    # Constava apenas como Itamaraca
    adm2_name = ifelse(gaul_adm2 == 9571, "Ilha De Itamaraca", adm2_name),
    # Constava como Pariquera-acu
    adm2_name = ifelse(gaul_adm2 == 11387, "Pariquera-Acu", adm2_name))

# Adiciona rótulos Gaul1
gaul2 <- gaul2 %>% 
  left_join(gaul1, by = "gaul_adm1")

# Contagem de municípios por Estado
mun_por_estado <- gaul2 %>%
  count(adm1_name, sort = T)

# Salva arquivo no disco
gaul2 %>% 
  write_csv("data/gaul_br_adm1e2.csv")
