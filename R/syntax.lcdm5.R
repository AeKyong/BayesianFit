syntax.lcdm5 = "
    item1	~ A2 + A4 + A5 + A2:A4 + A2:A5 + A4:A5 + A2:A4:A5
    item2	~ A1 + A5 + A1:A5
    item3	~ A2 + A5 + A2:A5
    item4	~ A3 + A4 + A5 + A3:A4 + A3:A5 + A4:A5 + A3:A4:A5
    item5	~ A5
    item6	~ A4 + A5 + A4:A5
    item7	~ A4 + A5 + A4:A5
    item8	~ A3 + A5 + A3:A5
    item9	~ A2 + A5 + A2:A5
    item10	~ A1
    item11	~ A2
    item12	~ A1 + A2 + A5 + A1:A2 + A1:A5 + A2:A5 + A1:A2:A5
    item13	~ A4 + A5 + A4:A5
    item14	~ A3
    item15	~ A2
    item16	~ A2
    item17	~ A2
    item18	~ A1 + A5 + A1:A5
    item19	~ A4 + A5 + A4:A5
    item20	~ A4

    A1 A2 A3 A4 A5 <- latent(distribution='mvbernoulli', structure='joint')
    item1-item20 <- observed(distribution='bernoulli', link='logit')
"
