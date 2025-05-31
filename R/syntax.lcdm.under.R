syntax.lcdm.under = "
    item1	~ A1
    item2	~ A1
    item3	~ A1
    item4	~ A1
    item5	~ A2
    item6	~ A2
    item7	~ A2
    item8	~ A2
    item9	~ A3
    item10	~ A3
    item11	~ A3
    item12	~ A3
    item13	~ A2
    item14	~ A1
    item15	~ A1 + A2 + A1:A2
    item16	~ A1 + A2 + A1:A2
    item17	~ A3
    item18	~ A1
    item19	~ A1 + A3 + A1:A3
    item20	~ A1 + A3 + A1:A3
    item21	~ A3
    item22	~ A2
    item23	~ A2 + A3 + A2:A3
    item24	~ A2 + A3 + A2:A3
    item25	~ A2
    item26	~ A2 + A3 + A2:A3
    item27	~ A1
    item28	~ A1 + A3 + A1:A3

    A1 A2 A3 <- latent(distribution='mvbernoulli', structure='joint')
    item1-item28 <- observed(distribution = 'bernoulli', link = 'logit')
    "
