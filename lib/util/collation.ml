(* Weights of the default table of the Unicode collation algorithm (DUCET,
   allkeys.txt 17.0.0) of the lowercase or caseless letters and of the
   combining marks of the Latin, Greek and Cyrillic blocks, ligatures,
   superscript and fullwidth forms included, without canonical decomposition
   or weighed apart from it (й): code point, primary and secondary weights, in
   hexadecimal; the primary weights are multiplied by 16, so that a tailoring
   can insert letters between two of them *)
let ducet =
  {|61:23EC:20 62:2406:20 63:2420:20 64:2436:20 65:2453:20 66:248E:20 67:249D:20
68:24C4:20 69:24DF:20 6A:24FB:20 6B:2514:20 6C:2528:20 6D:2562:20 6E:2571:20
6F:2598:20 70:25C8:20 71:25DD:20 72:25F0:20 73:2632:20 74:265D:20 75:2680:20
76:26B0:20 77:26C2:20 78:26CC:20 79:26D8:20 7A:26EE:20 AA:23EC:20 B5:279F:20
BA:2598:20 DF:2632.2632:20.11F.20 E6:23EC.2453:20.11F.20 F0:2436:20.11F
F8:2598:20.2F FE:271E:20 111:2436:20.39 127:24C4:20.39 131:24E3:20
133:24DF.24FB:20.20 138:25EC:20 140:2528:20.11F 142:2528:20.39
149:274E.2571:20.20 14B:2591:20 153:2598.2453:20.11F.20 167:2662:20
17F:2632:20.120 180:240E:20 183:241B:20 185:2741:20 188:242C:20 18C:2449:20
18D:26EE.26C2:20.20 192:2497:20 195:24CC:20 199:251A:20 19A:2533:20
19B:2559:20 19E:2580:20 1A3:24C0:20 1A5:25D1:20 1A8:2739:20 1AA:264F:20
1AB:2668:20 1AD:266C:20 1B4:26E4:20 1B6:26F3:20 1B9:2710:20 1BA:2716:20
1BB:2732:20 1BD:273D:20 1BE:265D.2632:20.20 1BF:2725:20 1C0:276B:20
1C1:276F:20 1C2:2773:20 1C3:2777:20 1C6:2436.26EE:20.20.28 1C9:2528.24FB:20.20
1CC:2571.24FB:20.20 1DD:2461:20 1E5:24AC:20 1F3:2436.26EE:20.20 21D:26EA:20
221:244D:20 223:25C3:20 225:26F9:20 234:254D:20 235:258B:20 236:2676:20
237:24FF:20 238:2436.2406:20.20 239:25DD.25C8:20.20 23C:2425:20 23F:2641:20
240:2705:20 242:2749:20 247:245A:20 249:2504:20 24B:25E8:20 24D:25FB:20
24F:26E0:20 250:23F7:20 251:23FB:20 252:2401:20 253:2417:20 254:25AC:20
255:2430:20 256:243F:20 257:2443:20 258:2470:20 259:2466:20 25A:2474:20
25B:246B:20 25C:2478:20 25D:247E:20 25E:2482:20 25F:250C:20 260:24B1:20
261:24A2:20 262:24A8:20 263:24BC:20 264:248A:20 265:2695:20 266:24D1:20
267:24D9:20 268:24EE:20 269:24F6:20 26A:24E7:20 26B:2538:20 26C:253E:20
26D:2546:20 26E:2552:20 26F:26A1:20 270:26A7:20 271:2569:20 272:257C:20
273:2586:20 274:2575:20 275:25B9:20 276:259F:20 277:25BE:20 278:25D8:20
279:2600:20 27A:2605:20 27B:260A:20 27C:2610:20 27D:2614:20 27E:261A:20
27F:2620:20 280:25F5:20 281:2629:20 282:263B:20 283:2648:20 284:2510:20
285:2653:20 286:2657:20 287:267B:20 288:2670:20 289:268B:20 28A:26AB:20
28B:26B7:20 28C:26BE:20 28D:26C8:20 28E:255D:20 28F:26DC:20 290:26FD:20
291:2701:20 292:270B:20 293:271A:20 294:2745:20 295:2754:20 296:2766:20
297:277C:20 298:2781:20 299:240A:20 29A:2486:20 29B:24B5:20 29C:24C8:20
29D:2508:20 29E:2523:20 29F:252C:20 2A0:25E4:20 2A1:275E:20 2A2:2762:20
2A3:2436.26EE:20.20 2A4:2436.270B:20.20 2A5:2436.2701:20.20
2A6:265D.2632:20.20 2A7:265D.2648:20.20 2A8:265D.2430:20.20
2A9:248E.2591:20.20 2AA:2528.2632:20.20 2AB:2528.26EE:20.20 2AC:2785:20
2AD:2789:20 2AE:2699:20 2AF:269D:20 2B0:24C4:20 2B1:24D1:20 2B2:24FB:20
2B3:25F0:20 2B4:2600:20 2B5:260A:20 2B6:2629:20 2B7:26C2:20 2B8:26D8:20
2BB:24DD:20 2BC:274E:20 2BD:24DE:20 2BE:2750:20 2BF:2759:20 2C0:274D:20
2C1:275A:20 2E0:24BC:20 2E1:2528:20 2E2:2632:20 2E3:26CC:20 2E4:2754:20
2EE:274F:20 300::25 301::24 302::27 303::2D 304::32 305::3A 306::26 307::2E
308::2B 309::3B 30A::29 30B::2C 30C::28 30D::33 30E::33 30F::3C 310::3D
311::3E 312::33 313::22 314::23 315::33 316::34 317::34 318::34 319::34
31A::33 31B::3F 31C::34 31D::34 31E::34 31F::34 320::34 321::40 322::41
323::42 324::43 325::44 326::45 327::30 328::31 329::34 32A::34 32B::34
32C::34 32D::46 32E::47 32F::34 330::48 331::49 332::21 333::34 334::4A
335::39 336::35 337::35 338::2F 339::4B 33A::34 33B::34 33C::34 33D::33
33E::33 33F::33 342::2A 345::4C 346::33 347::34 348::34 349::34 34A::33
34B::33 34C::33 34D::34 34E::34 350::33 351::33 352::33 353::34 354::34
355::34 356::34 357::33 358::4D 359::34 35A::34 35B::33 35C::34 35D::33
35E::33 35F::34 360::4E 361::4F 362::34 363:23EC:20 364:2453:20 365:24DF:20
366:2598:20 367:2680:20 368:2420:20 369:2436:20 36A:24C4:20 36B:2562:20
36C:25F0:20 36D:265D:20 36E:26B0:20 36F:26CC:20 371:2797:20 373:27B8:20
377:2794:20 37A:279A:20 37B:27AD:20 37C:27AC:20 37D:27AE:20 3B1:278D:20
3B2:278E:20 3B3:278F:20 3B4:2791:20 3B5:2792:20 3B6:2796:20 3B7:2798:20
3B8:2799:20 3B9:279A:20 3BA:279C:20 3BB:279D:20 3BC:279F:20 3BD:27A0:20
3BE:27A1:20 3BF:27A2:20 3C0:27A3:20 3C1:27A8:20 3C2:27AB:20 3C3:27AB:20
3C4:27AF:20 3C5:27B0:20 3C6:27B1:20 3C7:27B2:20 3C8:27B3:20 3C9:27B5:20
3D0:278E:20 3D1:2799:20 3D2:27B0:20 3D5:27B1:20 3D6:27A3:20
3D7:279C.278D.279A:20.20.20 3D9:27A7:20 3DB:2795:20 3DD:2793:20 3DF:27A6:20
3E1:27B7:20 3E3:27D9:20 3E5:27DE:20 3E7:27DF:20 3E9:27E2:20 3EB:27E9:20
3ED:27EC:20 3EF:27F0:20 3F0:279C:20 3F1:27A8:20 3F2:27AB:20 3F3:279B:20
3F5:2792:20 3F8:27B9:20 3FB:27A5:20 3FC:27AA:20 430:27F6:20 431:2802:20
432:2806:20 433:280A:20 434:281E:20 435:282E:20 436:2836:20 437:2840:20
438:2854:20 439:2861:20 43A:286A:20 43B:2884:20 43C:2897:20 43D:28A0:20
43E:28BB:20 43F:28C3:20 440:28D0:20 441:28D9:20 442:28E2:20 443:28F2:20
444:2903:20 445:2907:20 446:292E:20 447:2939:20 448:2958:20 449:295D:20
44A:2964:20 44B:2969:20 44C:296D:20 44D:297A:20 44E:297E:20 44F:2984:20
452:2824:20 454:2832:20 455:2848:20 456:285C:20 458:2865:20 459:2890:20
45A:28B6:20 45B:28EE:20 45F:2954:20 461:291D:20 463:2975:20 465:2989:20
467:298D:20 469:2997:20 46B:2992:20 46D:299C:20 46F:29A0:20 471:29A4:20
473:29A8:20 475:29AC:20 479:28FF:20 47B:292A:20 47D:2926:20 47F:2921:20
481:28CC:20 483::50 484::33 485::23 486::22 487::33 48B:2858:20 48D:2971:20
48F:28D4:20 491:280A:20.120 493:280E:20 495:2816:20 497:283C:20 499:282A:20
49B:286E:20 49D:287E:20 49F:287A:20 4A1:2876:20 4A3:28A9:20 4A5:28B2:20
4A7:28C8:20 4A9:29B1:20 4AB:28DE:20 4AD:28E9:20 4AF:28F6:20 4B1:28FA:20
4B3:2913:20 4B5:2934:20 4B7:293F:20 4B9:2947:20 4BB:2917:20 4BD:294C:20
4BF:2950:20 4C4:2872:20 4C6:2889:20 4C8:28AD:20 4CA:28A5:20 4CC:2943:20
4CE:289B:20 4CF:29B6:20 4D5:27FE:20 4D9:27FA:20 4E1:284D:20 4E9:28BF:20
4F7:281A:20 4FB:2812:20 4FD:290B:20 4FF:290F:20 501:2822:20 503:2829:20
505:2845:20 507:2852:20 509:2895:20 50B:28BA:20 50D:28DD:20 50F:28E8:20
511:2846:20 513:288E:20 515:2896:20 517:28D8:20 519:2988:20 51B:2883:20
51D:29B5:20 51F:2882:20 521:288F:20 523:28B1:20 525:28C7:20 527:291B:20
529:28A4:20 52B:283A:20 52D:293D:20 52F:288D:20 1AB0::33 1AB1::33 1AB2::33
1AB3::33 1AB4::33 1AB5::34 1AB6::34 1AB7::34 1AB8::34 1AB9::34 1ABA::34
1ABB::33 1ABC::33 1ABD::34 1ABE::36 1ABF:26C2:20 1AC0:26C8:20 1AC1::33
1AC2::33 1AC3::34 1AC4::34 1AC5::33 1AC6::33 1AC7::33 1AC8::33 1AC9::33
1ACA::34 1ACB::33 1ACC:249D:20.120 1ACD:25F0:20.120 1ACE:265D:20.120 1ACF::33
1AD0::33 1AD1::33 1AD2::33 1AD3::33 1AD4::33 1AD5::33 1AD6::33 1AD7::33
1AD8::33 1AD9::33 1ADA::33 1ADB::33 1ADC::33 1ADD::34 1AE0::33 1AE1::33
1AE2::33 1AE3::33 1AE4::33 1AE5::33 1AE6::34 1AE7::33 1AE8::33 1AE9::33
1AEA::33 1AEB::33 1C80:2806:20 1C81:281E:20 1C82:28BB:20 1C83:28D9:20
1C84:28E2:20 1C85:28E2:20 1C86:2964:20 1C87:2975:20 1C88:28FE:20 1C8A:28E7:20
1D00:23F0:20 1D01:23F4:20 1D02:23F5:20 1D03:2413:20 1D04:2424:20 1D05:243A:20
1D06:243B:20 1D07:2457:20 1D08:247D:20 1D09:24ED:20 1D0A:2503:20 1D0B:2518:20
1D0C:2531:20 1D0D:2566:20 1D0E:257A:20 1D0F:259C:20 1D10:25B0:20 1D11:259D:20
1D12:25B1:20 1D13:25A9:20 1D14:25A3:20 1D15:25C7:20 1D16:25B6:20 1D17:25B7:20
1D18:25CC:20 1D19:25FA:20 1D1A:2604:20 1D1B:2661:20 1D1C:2684:20 1D1D:2686:20
1D1E:2687:20 1D1F:26A6:20 1D20:26B4:20 1D21:26C6:20 1D22:26F2:20 1D23:270F:20
1D24:275B:20 1D25:275C:20 1D26:2790:20 1D27:279E:20 1D28:27A4:20 1D29:27A9:20
1D2A:27B4:20 1D2B:2888:20 1D2C:23EC:20 1D2D:23EC.2453:20.11F.20 1D2E:2406:20
1D2F:2412:20 1D30:2436:20 1D31:2453:20 1D32:2461:20 1D33:249D:20 1D34:24C4:20
1D35:24DF:20 1D36:24FB:20 1D37:2514:20 1D38:2528:20 1D39:2562:20 1D3A:2571:20
1D3B:2579:20 1D3C:2598:20 1D3D:25C3:20 1D3E:25C8:20 1D3F:25F0:20 1D40:265D:20
1D41:2680:20 1D42:26C2:20 1D43:23EC:20 1D44:23F7:20 1D45:23FB:20 1D46:23F5:20
1D47:2406:20 1D48:2436:20 1D49:2453:20 1D4A:2466:20 1D4B:246B:20 1D4C:247D:20
1D4D:249D:20 1D4E:24ED:20 1D4F:2514:20 1D50:2562:20 1D51:2591:20 1D52:2598:20
1D53:25AC:20 1D54:25B6:20 1D55:25B7:20 1D56:25C8:20 1D57:265D:20 1D58:2680:20
1D59:2686:20 1D5A:26A1:20 1D5B:26B0:20 1D5C:275C:20 1D5D:278E:20 1D5E:278F:20
1D5F:2791:20 1D60:27B1:20 1D61:27B2:20 1D62:24DF:20 1D63:25F0:20 1D64:2680:20
1D65:26B0:20 1D66:278E:20 1D67:278F:20 1D68:27A8:20 1D69:27B1:20 1D6A:27B2:20
1D6B:2688:20 1D6C:2414:20 1D6D:243D:20 1D6E:2495:20 1D6F:2567:20 1D70:257B:20
1D71:25CF:20 1D72:25FF:20 1D73:261E:20 1D74:2639:20 1D75:2667:20 1D76:26F7:20
1D77:24B9:20 1D78:28A0:20 1D79:249D:20.120 1D7A:265D.24C4:20.11F.20
1D7B:24F2:20 1D7C:24FA:20 1D7D:25CD:20 1D7E:2691:20 1D7F:26AF:20 1D80:2416:20
1D81:243E:20 1D82:2496:20 1D83:24B0:20 1D84:2519:20 1D85:2545:20 1D86:2568:20
1D87:2585:20 1D88:25D0:20 1D89:260F:20 1D8A:263A:20 1D8B:264D:20 1D8C:26B6:20
1D8D:26D0:20 1D8E:26F8:20 1D8F:23F2:20 1D90:2400:20 1D91:2448:20 1D92:245E:20
1D93:246F:20 1D94:247C:20 1D95:246A:20 1D96:24F4:20 1D97:25B3:20 1D98:264E:20
1D99:2692:20 1D9A:2715:20 1D9B:2401:20 1D9C:2420:20 1D9D:2430:20
1D9E:2436:20.11F 1D9F:2478:20 1DA0:248E:20 1DA1:250C:20 1DA2:24A2:20
1DA3:2695:20 1DA4:24EE:20 1DA5:24F6:20 1DA6:24E7:20 1DA7:24F2:20 1DA8:2508:20
1DA9:2546:20 1DAA:2545:20 1DAB:252C:20 1DAC:2569:20 1DAD:26A7:20 1DAE:257C:20
1DAF:2586:20 1DB0:2575:20 1DB1:25B9:20 1DB2:25D8:20 1DB3:263B:20 1DB4:2648:20
1DB5:2668:20 1DB6:268B:20 1DB7:26AB:20 1DB8:2684:20 1DB9:26B7:20 1DBA:26BE:20
1DBB:26EE:20 1DBC:26FD:20 1DBD:2701:20 1DBE:270B:20 1DBF:2799:20 1DC0::33
1DC1::33 1DC2::34 1DC3::33 1DC4::33 1DC5::33 1DC6::33 1DC7::33 1DC8::33
1DC9::33 1DCA:25F0:20 1DCB::33 1DCC::33 1DCD::33 1DCE::33 1DCF::34 1DD0::34
1DD1::33 1DD2:2730:20 1DD3:23EC:20.11F 1DD4:23EC.2453:20.11F.20
1DD5:23EC.2598:20.20 1DD6:23EC.26B0:20.20 1DD7:2420:20.30 1DD8:2436:20.120
1DD9:2436:20.11F 1DDA:249D:20 1DDB:24A8:20 1DDC:2514:20 1DDD:2528:20
1DDE:252C:20 1DDF:2566:20 1DE0:2571:20 1DE1:2575:20 1DE2:25F5:20
1DE3:25F0:20.121 1DE4:2632:20 1DE5:2632:20.120 1DE6:26EE:20 1DE7:23FB:20
1DE8:2406:20 1DE9:241F:20 1DEA:2466:20 1DEB:248E:20 1DEC:253C:20
1DED:2598:20.34 1DEE:25C8:20 1DEF:2648:20 1DF0:2680:20.34 1DF1:26C2:20
1DF2:23EC:20.2B 1DF3:2598:20.2B 1DF4:2680:20.2B 1DF5::33 1DF6::33 1DF7::33
1DF8::33 1DF9::34 1DFA::34 1DFB::33 1DFC::34 1DFD::34 1DFE::33 1DFF::34
1E9A:23EC.2750:20.20 1E9C:2646:20 1E9D:2647:20 1E9F:2452:20
1EFB:2528.2528:20.20 1EFD:26BD:20 1EFF:26E8:20 2071:24DF:20 207F:2571:20
2090:23EC:20 2091:2453:20 2092:2598:20 2093:26CC:20 2094:2466:20 2095:24C4:20
2096:2514:20 2097:2528:20 2098:2562:20 2099:2571:20 209A:25C8:20 209B:2632:20
209C:265D:20 20D0::111 20D1::112 20D2::113 20D3::113 20D4::114 20D5::115
20D6::116 20D7::117 20D8::35 20D9::35 20DA::35 20DB::118 20DC::119 20DD::36
20DE::36 20DF::36 20E0::36 20E1::11A 20E2::36 20E3::36 20E4::36 20E5::35
20E6::11B 20E7::11C 20E8::11D 20E9::11E 20EA::35 20EB::35 20EC::34 20ED::34
20EE::34 20EF::34 20F0::33 2C61:2537:20 2C65:23F1:20 2C66:2666:20 2C68:24D5:20
2C6A:251E:20 2C6C:2709:20 2C71:26BB:20 2C73:26C7:20 2C74:26BC:20 2C76:24D6:20
2C77:25DC:20 2C78:2460:20 2C79:260E:20 2C7A:25B8:20 2C7B:2465:20 2C7C:24FB:20
2C7D:26B0:20 2DE0:2802:20 2DE1:2806:20 2DE2:280A:20 2DE3:281E:20 2DE4:2836:20
2DE5:2840:20 2DE6:286A:20 2DE7:2884:20 2DE8:2897:20 2DE9:28A0:20 2DEA:28BB:20
2DEB:28C3:20 2DEC:28D0:20 2DED:28D9:20 2DEE:28E2:20 2DEF:2907:20 2DF0:292E:20
2DF1:2939:20 2DF2:2958:20 2DF3:295D:20 2DF4:29A8:20 2DF5:28D9.28E2:20.20
2DF6:27F6:20 2DF7:282E:20 2DF8:2869:20 2DF9:28FE:20 2DFA:2975:20 2DFB:297E:20
2DFC:2983:20 2DFD:298D:20 2DFE:2992:20 2DFF:299C:20 A641:2844:20 A643:2847:20
A645:284C:20 A647:2860:20 A649:2869:20 A64B:28FE:20 A64D:2925:20 A64F:2961:20
A651:2968:20 A653:2979:20 A655:2982:20 A657:2983:20 A659:2991:20 A65B:2996:20
A65D:299B:20 A65F:29B0:20 A661:2932:20 A663:2828:20 A665:2894:20 A667:289F:20
A669:28BB:20 A66B:28BB:20 A66D:28BB:20 A66E:28BB:20 A66F::51 A674:2832:20
A675:2854:20 A676:285C:20.2B A677:28F2:20 A678:2964:20 A679:2969:20
A67A:296D:20 A67B:291D:20 A67C::33 A67D::33 A67F:2963:20 A681:2823:20
A683:2853:20 A685:283B:20 A687:294B:20 A689:2851:20 A68B:28ED:20 A68D:28E6:20
A68F:2933:20 A691:2938:20 A693:293E:20 A695:291C:20 A697:295C:20 A699:28BB:20
A69B:28BB:20 A69C:2964:20 A69D:296D:20 A69E:2903:20 A69F:2989:20 A723:2751:20
A725:275D:20 A727:24D8:20 A729:265D.26EE:20.20 A72B:2736:20 A72D:2737:20
A72F:2738:20 A730:2492:20 A731:2636:20 A733:23EC.23EC:20.20
A735:23EC.2598:20.20 A737:23EC.2680:20.20 A739:23EC.26B0:20.20
A73B:23EC.26B0:20.11F.20 A73D:23EC.26D8:20.20 A73F:2435:20 A741:251F:20
A743:2520:20 A745:2521:20 A747:2530:20 A749:2532:20 A74B:25BD:20 A74D:25B5:20
A74F:2598.2598:20.20 A751:25CE:20 A753:25D5:20 A755:25D6:20 A757:25E2:20
A759:25E3:20 A75B:25F0:20.121 A75D:2631:20 A75F:26B5:20 A761:26B0.26D8:20.20
A763:270A:20 A765:2723:20 A767:2724:20 A769:272A:20 A76B:272E:20 A76D:272F:20
A76F:2730:20 A770:2730:20 A771:2451:20 A772:2551:20 A773:2570:20 A774:2590:20
A775:262F:20 A776:2630:20 A777:267A:20 A778:2731:20 A77A:2436:20.120
A77C:248E:20.120 A77F:24BB:20 A781:2557:20 A783:25F0:20.120 A785:2632:20.121
A787:265D:20.120 A78C:2752:20 A78E:254B:20 A78F:2753:20 A791:2584:20
A793:2429:20 A794:242A:20 A795:24D0:20 A797:2415:20 A799:2494:20
A79B:23EC:20.2B A79D:2598:20.2B A79F:2680:20.2B A7A1:249D:20.35
A7A3:2514:20.35 A7A5:2571:20.35 A7A7:25F0:20.35 A7A9:2632:20.35 A7AF:25E1:20
A7B5:241F:20 A7B7:25C2:20 A7B9:2690:20 A7BB:23F3:20 A7BD:24F5:20 A7BF:2694:20
A7C1:23EC:20.31 A7C3:26C2:20.11F A7C8:243C:20 A7CA:2637:20 A7CD:2638:20
A7CF:2758:20 A7D1:24A1:20 A7D3:2722:20 A7D5:2729:20 A7D7:2645:20
A7D9:2632:20.122 A7DB:2558:20 A7F1:2632:20 A7F2:2420:20 A7F3:248E:20
A7F4:25DD:20 A7F6:24D7:20 A7F7:24EC:20 A7F8:24C4:20.39
A7F9:2598.2453:20.11F.20 A7FA:26A5:20 A7FB:249C:20 A7FC:25D7:20 A7FD:256E:20
A7FE:24EB:20 A7FF:256F:20 AB30:23FF:20 AB31:23F6:20 AB32:2458:20 AB33:2459:20
AB34:245F:20 AB35:2493:20 AB36:24A7:20 AB37:2544:20 AB38:253C:20 AB39:253D:20
AB3A:256D:20 AB3B:258F:20 AB3C:2595:20 AB3D:259E:20 AB3E:25AA:20 AB3F:25B2:20
AB40:25A6:20 AB41:25A4:20 AB42:25A5:20 AB43:25A7:20 AB44:25A8:20 AB45:25F4:20
AB46:25F9:20 AB47:2624:20 AB48:2625:20 AB49:2619:20 AB4A:2626:20 AB4B:2627:20
AB4C:2628:20 AB4D:264C:20 AB4E:2685:20 AB4F:268F:20 AB50:2689:20 AB51:268A:20
AB52:2693:20 AB53:26D5:20 AB54:26D6:20 AB55:26D7:20 AB56:26D1:20 AB57:26D2:20
AB58:26D3:20 AB59:26D4:20 AB5A:26E9:20 AB5C:24D8:20 AB5D:2544:20 AB5E:2538:20
AB5F:2693:20 AB60:272B:20 AB61:272C:20 AB62:25B4:20 AB63:272D:20 AB64:2405:20
AB65:27B6:20 AB66:2436.26FD:20.20 AB67:265D.263B:20.20 AB68:262D:20
AB69:26C8:20 FB00:248E.248E:20.20 FB01:248E.24DF:20.20 FB02:248E.2528:20.20
FB03:248E.248E.24DF:20.20.20 FB04:248E.248E.2528:20.20.20
FB05:2632.265D:20.120.20 FB06:2632.265D:20.20 FE20::4F FE22::4E FE27::34
FE29::4E FE2E::50 FF41:23EC:20 FF42:2406:20 FF43:2420:20 FF44:2436:20
FF45:2453:20 FF46:248E:20 FF47:249D:20 FF48:24C4:20 FF49:24DF:20 FF4A:24FB:20
FF4B:2514:20 FF4C:2528:20 FF4D:2562:20 FF4E:2571:20 FF4F:2598:20 FF50:25C8:20
FF51:25DD:20 FF52:25F0:20 FF53:2632:20 FF54:265D:20 FF55:2680:20 FF56:26B0:20
FF57:26C2:20 FF58:26CC:20 FF59:26D8:20 FF5A:26EE:20|}

let weights =
  let t = Hashtbl.create 2048 in
  let hex s = int_of_string ("0x" ^ s) in
  let ints s =
    if s = "" then [] else List.map hex (String.split_on_char '.' s)
  in
  String.split_on_char '\n' ducet
  |> List.concat_map (String.split_on_char ' ')
  |> List.iter (fun e ->
      match String.split_on_char ':' e with
      | [ c; p; s ] ->
          Hashtbl.replace t (hex c)
            (List.map (fun w -> w lsl 4) (ints p), ints s)
      | _ -> ());
  t

let ascii =
  Array.init 128 (fun c ->
      match
        Hashtbl.find_opt weights (Char.code (Char.lowercase_ascii (Char.chr c)))
      with
      | Some (p, _) -> p
      | None -> [ c lsl 4 ])

(* Standard tailorings of the Common Locale Data Repository (CLDR 50,
   common/collation/<language>.xml) for the languages of GeneWeb and the
   European languages with digraphs whose order differs from the common one *)
let rules =
  [
    ("af", {|&N<<<ŉ|});
    ("bg", {|[reorder Cyrl]|});
    ( "br",
      {|&C<ch<<<Ch<<<CH<c''h=c\u02BCh=c\u2019h<<<C''h=C\u02BCh=C\u2019h<<<C''H=C\u02BCH=C\u2019H
       &ker<<ꝃ<<<Ꝃ|}
    );
    ( "cs",
      {|&C<č<<<Č
       &H<ch<<<cH<<<Ch<<<CH
       &R<ř<<<Ř
       &S<š<<<Š
       &Z<ž<<<Ž|}
    );
    ( "cy",
      {|&C<ch<<<Ch<<<CH
       &D<dd<<<Dd<<<DD
       &F<ff<<<Ff<<<FF
       &G<ng<<<Ng<<<NG
       &L<ll<<<Ll<<<LL
       &P<ph<<<Ph<<<PH
       &R<rh<<<Rh<<<RH
       &T<th<<<Th<<<TH|}
    );
    ( "da",
      {|[caseFirst upper]
       &D<<đ<<<Đ<<ð<<<Ð
       &th<<<þ
       &TH<<<Þ
       &Y<<ü<<<Ü<<ű<<<Ű
       &[before 1]ǀ<æ<<<Æ<<ä<<<Ä<ø<<<Ø<<ö<<<Ö<<ő<<<Ő<å<<<Å<<<aa<<<Aa<<<AA
       &oe<<œ<<<Œ|}
    );
    ( "dsb",
      {|&C<č<<<Č<ć<<<Ć
       &E<ě<<<Ě
       &H<ch<<<cH<<<Ch<<<CH
       &[before 1] L<ł<<<Ł
       &N<ń<<<Ń
       &R<ŕ<<<Ŕ
       &S<š<<<Š<ś<<<Ś
       &Z<ž<<<Ž<ź<<<Ź|}
    );
    ( "eo",
      {|&C<ĉ<<<Ĉ
       &G<ĝ<<<Ĝ
       &H<ĥ<<<Ĥ
       &J<ĵ<<<Ĵ
       &S<ŝ<<<Ŝ
       &U<ŭ<<<Ŭ|}
    );
    ("es", {|&N<ñ<<<Ñ|});
    ( "et",
      {|&[before 1]T<š<<<Š<z<<<Z<ž<<<Ž
       &[before 1]X<õ<<<Õ<ä<<<Ä<ö<<<Ö<ü<<<Ü|}
    );
    ( "fi",
      {|&D\u0335<<đ<<<Đ
       &G\u0335<<ǥ<<<Ǥ
       &N\u0335<<ŋ<<<Ŋ
       &T\u0335<<ŧ<<<Ŧ
       &Y<<ü<<<Ü
       &Z\u0335<<ʒ<<<Ʒ
       &[before 1]ǀ<å<<<Å<ä<<<Ä<<æ<<<Æ<ö<<<Ö<<ø<<<Ø|}
    );
    ( "fo",
      {|&D<<đ<<<Đ<<ð<<<Ð
       &t<<<þ/h
       &T<<<Þ/H
       &Y<<ü<<<Ü<<ű<<<Ű
       &[before 1]ǀ<æ<<<Æ<<ä<<<Ä<<ę<<<Ę<ø<<<Ø<<ö<<<Ö<<ő<<<Ő<<œ<<<Œ<å<<<Å<<<aa<<<Aa<<<AA|}
    );
    ( "hr",
      {|[reorder Latn Cyrl]
       &C<č<<<Č<ć<<<Ć
       &D<dž<<<ǆ<<<Dž<<<ǅ<<<DŽ<<<Ǆ<đ<<<Đ
       &L<lj<<<ǉ<<<Lj<<<ǈ<<<LJ<<<Ǉ
       &N<nj<<<ǌ<<<Nj<<<ǋ<<<NJ<<<Ǌ
       &S<š<<<Š
       &Z<ž<<<Ž|}
    );
    ( "hsb",
      {|&C<č<<<Č<ć<<<Ć
       &E<ě<<<Ě
       &H<ch<<<cH<<<Ch<<<CH
       &[before 1] L<ł<<<Ł
       &R<ř<<<Ř
       &S<š<<<Š
       &Z<ž<<<Ž<ź<<<Ź|}
    );
    ( "hu",
      {|&C<cs<<<Cs<<<CS
       &D<dz<<<Dz<<<DZ
       &DZ<dzs<<<Dzs<<<DZS
       &G<gy<<<Gy<<<GY
       &L<ly<<<Ly<<<LY
       &N<ny<<<Ny<<<NY
       &S<sz<<<Sz<<<SZ
       &T<ty<<<Ty<<<TY
       &Z<zs<<<Zs<<<ZS
       &O<ö<<<Ö<<ő<<<Ő
       &U<ü<<<Ü<<ű<<<Ű
       &cs<<<ccs/cs
       &Cs<<<Ccs/cs
       &CS<<<CCS/CS
       &dz<<<ddz/dz
       &Dz<<<Ddz/dz
       &DZ<<<DDZ/DZ
       &dzs<<<ddzs/dzs
       &Dzs<<<Ddzs/dzs
       &DZS<<<DDZS/DZS
       &gy<<<ggy/gy
       &Gy<<<Ggy/gy
       &GY<<<GGY/GY
       &ly<<<lly/ly
       &Ly<<<Lly/ly
       &LY<<<LLY/LY
       &ny<<<nny/ny
       &Ny<<<Nny/ny
       &NY<<<NNY/NY
       &sz<<<ssz/sz
       &Sz<<<Ssz/sz
       &SZ<<<SSZ/SZ
       &ty<<<tty/ty
       &Ty<<<Tty/ty
       &TY<<<TTY/TY
       &zs<<<zzs/zs
       &Zs<<<Zzs/zs
       &ZS<<<ZZS/ZS|}
    );
    ( "is",
      {|&[before 1]b<á<<<Á
       &d<<đ<<<Đ<ð<<<Ð
       &[before 1]f<é<<<É
       &[before 1]j<í<<<Í
       &[before 1]p<ó<<<Ó
       &[before 1]v<ú<<<Ú
       &[before 1]z<ý<<<Ý
       &[before 1]ǀ<æ<<<Æ<<ä<<<Ä<ö<<<Ö<<ø<<<Ø<å<<<Å|}
    );
    ( "lt",
      {|&̀=̇̀
       &́=̇́
       &̃=̇̃
       &A<<ą<<<Ą
       &C<č<<<Č
       &E<<ę<<<Ę<<ė<<<Ė
       &I<<į<<<Į<<y<<<Y
       &S<š<<<Š
       &U<<ų<<<Ų<<ū<<<Ū
       &Z<ž<<<Ž|}
    );
    ( "lv",
      {|&A<ā<<<Ā
       &C<č<<<Č
       &E<ē<<<Ē
       &G<ģ<<<Ģ
       &I<y<<<Y<ī<<<Ī
       &K<ķ<<<Ķ
       &L<ļ<<<Ļ
       &N<ņ<<<Ņ
       &O<ō<<<Ō
       &R<ŗ<<<Ŗ
       &S<š<<<Š
       &U<ū<<<Ū
       &Z<ž<<<Ž|}
    );
    ( "mt",
      {|[caseFirst upper]
       &[before 1]c<ċ<<<Ċ
       &[before 1]g<ġ<<<Ġ
       &[before 1]h<għ<<<gĦ<<<Għ<<<GĦ
       &[before 1]i<ħ<<<Ħ
       &[before 1]z<ż<<<Ż|}
    );
    ( "no",
      {|&D<<đ<<<Đ<<ð<<<Ð
       &t<<<þ/h
       &T<<<Þ/H
       &Y<<ü<<<Ü<<ű<<<Ű
       &[before 1]ǀ<æ<<<Æ<<ä<<<Ä<<ę<<<Ę<ø<<<Ø<<ö<<<Ö<<ő<<<Ő<<œ<<<Œ<å<<<Å<<aa<<<Aa<<<AA|}
    );
    ( "pl",
      {|&A<ą<<<Ą
       &C<ć<<<Ć
       &E<ę<<<Ę
       &L<ł<<<Ł
       &N<ń<<<Ń
       &O<ó<<<Ó
       &S<ś<<<Ś
       &Z<ź<<<Ź<ż<<<Ż|}
    );
    ( "ro",
      {|&A<ă<<<Ă<â<<<Â
       &I<î<<<Î
       &S<ş=ș<<<Ş=Ș
       &T<ţ=ț<<<Ţ=Ț|}
    );
    ("ru", {|[reorder Cyrl]|});
    ( "sk",
      {|&A<ä<<<Ä
       &C<č<<<Č
       &H<ch<<<cH<<<Ch<<<CH
       &O<ô<<<Ô
       &R<ř<<<Ř
       &S<š<<<Š
       &Z<ž<<<Ž|}
    );
    ("sl", {|&C<č<<<Č<ć<<<Ć
       &D<đ<<<Đ
       &S<š<<<Š
       &Z<ž<<<Ž|});
    ( "sq",
      {|&[before 1]D<ç<<<Ç
       &[before 1]E<dh<<<Dh<<<DH
       &[before 1]F<ë<<<Ë
       &[before 1]H<gj<<<Gj<<<GJ
       &[before 1]M<ll<<<Ll<<<LL
       &[before 1]O<nj<<<Nj<<<NJ
       &[before 1]S<rr<<<Rr<<<RR
       &[before 1]T<sh<<<Sh<<<SH
       &[before 1]U<th<<<Th<<<TH
       &[before 1]Y<xh<<<Xh<<<XH
       &[before 1]Ʒ<zh<<<Zh<<<ZH|}
    );
    ( "sv",
      {|&D<<đ<<<Đ<<ð<<<Ð
       &t<<<þ/h
       &T<<<Þ/H
       &Y<<ü<<<Ü<<ű<<<Ű
       &[before 1]ǀ<å<<<Å<ä<<<Ä<<æ<<<Æ<<ę<<<Ę<ö<<<Ö<<ø<<<Ø<<ő<<<Ő<<œ<<<Œ<<ô<<<Ô|}
    );
    ( "tr",
      {|&C<ç<<<Ç
       &G<ğ<<<Ğ
       &[before 1]i<ı<<<I
       &i<<<İ
       &O<ö<<<Ö
       &S<ş<<<Ş
       &U<ü<<<Ü|}
    );
  ]

type tailoring = {
  items : (string, int list * int list) Hashtbl.t;
  mutable contractions : string list;
  mutable upper_first : bool;
  mutable cyrillic_first : bool;
}

let root =
  {
    items = Hashtbl.create 1;
    contractions = [];
    upper_first = false;
    cyrillic_first = false;
  }

let current = ref root

let decompose s =
  let n = Uunf.create `NFD in
  let us = ref [] in
  let rec add v =
    match Uunf.add n v with
    | `Uchar u ->
        us := u :: !us;
        add `Await
    | `Await | `End -> ()
  in
  Uutf.String.fold_utf_8
    (fun () _ -> function `Uchar _ as u -> add u | `Malformed _ -> ())
    () s;
  add `End;
  List.rev !us

let lower u = match Uucp.Case.Map.to_lower u with `Uchars [ u ] -> u | _ -> u

let next s i =
  let n = try Utf8.nbc s.[i] with Invalid_argument _ -> 1 in
  min (String.length s) (i + n)

let rec marks s i =
  if i < String.length s && Char.code (String.unsafe_get s i) >= 0x80 then
    let d = String.get_utf_8_uchar s i in
    if Uunf.ccc (Uchar.utf_decode_uchar d) > 0 then
      marks s (i + Uchar.utf_decode_length d)
    else i
  else i

(* the primary and secondary weights of the character at [i] in [s] and of
   the combining marks following it, decomposed together (NFD), and the
   position of the next character; the letters of other scripts after these
   ones, by code point, the other characters by their transliteration *)
let root_char t s i =
  let c = Char.code (String.unsafe_get s i) in
  let j = next s i in
  let k = marks s j in
  if c < 0x80 && k = j then (ascii.(c), [ 0x20 ], j)
  else
    let ch = String.sub s i (j - i) in
    let u = Uchar.utf_decode_uchar (String.get_utf_8_uchar s i) in
    let us =
      if Hashtbl.mem weights (Uchar.to_int (lower u)) then
        u :: decompose (String.sub s j (k - j))
      else decompose (String.sub s i (k - i))
    in
    let p, w =
      List.split
      @@ List.map
           (fun u ->
             let l = Uchar.to_int (lower u) in
             match Hashtbl.find_opt weights l with
             | Some (p, w) when t.cyrillic_first && Uucp.Script.script u = `Cyrl
               ->
                 (List.map (fun w -> w - 0x8000) p, w)
             | Some w -> w
             | None -> (
                 match Uucp.Gc.general_category u with
                 | `Lu | `Ll | `Lt | `Lm | `Lo ->
                     ([ (0x10000 + l) lsl 4 ], [ 0x20 ])
                 | `Mn | `Mc | `Me -> ([], [])
                 | _ ->
                     let tr, _ = Name.unaccent_utf_8 false ch 0 in
                     let p =
                       List.concat_map
                         (fun c -> ascii.(Char.code c land 0x7F))
                         (List.of_seq (String.to_seq tr))
                     in
                     (p, List.map (fun _ -> 0x20) p)))
           us
    in
    (List.concat p, List.concat w, k)

let matches s i k =
  let n = String.length k in
  i + n <= String.length s
  &&
  let rec loop j = j >= n || (s.[i + j] = k.[j] && loop (j + 1)) in
  loop 0

(* as [root_char], the contractions and letters of the tailoring [t] first *)
let char t s i =
  if t == root then root_char t s i
  else
    match List.find_opt (matches s i) t.contractions with
    | Some k ->
        let p, w = Hashtbl.find t.items k in
        (p, w, i + String.length k)
    | None -> (
        let j = next s i in
        let ch = String.sub s i (j - i) in
        match Hashtbl.find_opt t.items ch with
        | Some (p, w) -> (p, w, j)
        | None -> (
            match
              Hashtbl.find_opt t.items
                (if j - i = 1 then String.lowercase_ascii ch
                 else Utf8.cmap_utf_8 Uucp.Case.Map.to_lower ch)
            with
            | Some (p, w) -> (p, w, j)
            | None -> root_char t s i))

let weights_of t s =
  let rec loop i p w =
    if i >= String.length s then
      (List.concat (List.rev p), List.concat (List.rev w))
    else
      let p', w', i = char t s i in
      loop i (p' :: p) (w' :: w)
  in
  loop 0 [] []

let unescape r =
  let n = String.length r in
  let b = Buffer.create n in
  let rec loop i =
    if i < n then
      match r.[i] with
      | '\\' when i + 5 < n && r.[i + 1] = 'u' ->
          Uutf.Buffer.add_utf_8 b
            (Uchar.of_int (int_of_string ("0x" ^ String.sub r (i + 2) 4)));
          loop (i + 6)
      | '\'' when i + 1 < n && r.[i + 1] = '\'' ->
          Buffer.add_char b '\'';
          loop (i + 2)
      | c ->
          Buffer.add_char b c;
          loop (i + 1)
  in
  loop 0;
  Buffer.contents b

(* The tailoring of the rules [r]: resets [&x], [&[before 1]x], relations
   [<] (primary), [<<] (secondary), [<<<] and [=] (same weights, the case
   being compared apart), expansions [y/x], options [[caseFirst upper]] and
   [[reorder Cyrl]] *)
let tailor r =
  let t =
    {
      items = Hashtbl.create 64;
      contractions = [];
      upper_first = false;
      cyrillic_first = false;
    }
  in
  let r = Mutil.normalize_utf_8 (unescape r) in
  let n = String.length r in
  let blank c = c = ' ' || c = '\n' || c = '\t' || c = '\r' in
  let rec skip i = if i < n && blank r.[i] then skip (i + 1) else i in
  let word i =
    let i = skip i in
    let j = ref i in
    while !j < n && not (blank r.[!j] || String.contains "&<=/[" r.[!j]) do
      incr j
    done;
    (String.sub r i (!j - i), !j)
  in
  let bump p d =
    match List.rev p with w :: l -> List.rev ((w + d) :: l) | [] -> [ d ]
  in
  let rec loop i base cur sec =
    let i = skip i in
    if i < n then
      match r.[i] with
      | '[' ->
          let j = String.index_from r i ']' in
          (match String.sub r i (j - i + 1) with
          | "[caseFirst upper]" -> t.upper_first <- true
          | "[reorder Cyrl]" -> t.cyrillic_first <- true
          | _ -> ());
          loop (j + 1) base cur sec
      | '&' ->
          let i = skip (i + 1) in
          let before = i < n && r.[i] = '[' in
          let i = if before then String.index_from r i ']' + 1 else i in
          let x, i = word i in
          let p, w = weights_of t x in
          let x = ((if before then bump p (-16) else p), w) in
          loop i x x 0
      | c ->
          let k =
            if c = '=' then 1
            else if i + 2 < n && r.[i + 1] = '<' && r.[i + 2] = '<' then 3
            else if i + 1 < n && r.[i + 1] = '<' then 2
            else 1
          in
          let y, i = word (i + k) in
          let x, i = if i < n && r.[i] = '/' then word (i + 1) else ("", i) in
          let ((p, w) as v), base, sec =
            match (c, k) with
            | '<', 1 ->
                let p = bump (fst cur) 1 in
                let v = (p, List.map (fun _ -> 0x20) p) in
                (v, v, 0)
            | '<', 2 ->
                ((fst base, snd base @ [ 0x200 + sec + 1 ]), base, sec + 1)
            | _ -> (cur, base, sec)
          in
          let xp, xw = weights_of t x in
          Hashtbl.replace t.items y (p @ xp, w @ xw);
          if Utf8.length y > 1 then t.contractions <- y :: t.contractions;
          loop i base v sec
  in
  loop 0 ([], []) ([], []) 0;
  t.contractions <-
    List.sort
      (fun a b -> Int.compare (String.length b) (String.length a))
      t.contractions;
  t

(* the case weights of the characters of [s] from [i] to [j], weighing [n]
   primary weights: one for each character other than a combining mark, a
   titlecase one counting as uppercase then lowercase (ǅ), the last one
   repeated up to [n] (Ǆ, ß) *)
let cases up s i j n =
  let rec loop i acc =
    if i >= j then acc
    else
      let d = String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar d in
      let acc =
        if Uunf.ccc u > 0 then acc
        else if Uucp.Gc.general_category u = `Lt then 2 :: up :: acc
        else if Uucp.Case.is_upper u then up :: acc
        else 2 :: acc
      in
      loop (i + Uchar.utf_decode_length d) acc
  in
  match loop i [] with
  | [] -> []
  | last :: _ as l ->
      List.rev_append l (List.init (max 0 (n - List.length l)) (fun _ -> last))

(* The sort key of [s] (Unicode collation algorithm, 7.3): its primary,
   secondary and tertiary weights, the tertiary being the case of the
   letters, then [s] itself, so that only equal strings have equal keys *)
let key t s =
  let up = if t.upper_first then 1 else 8 in
  let rec loop i p w c =
    if i < String.length s then
      let p', w', j = char t s i in
      let c' =
        if j = i + 1 then [ (match s.[i] with 'A' .. 'Z' -> up | _ -> 2) ]
        else cases up s i j (List.length p')
      in
      loop j (p' :: p) (w' :: w) (c' :: c)
    else List.(concat (rev p), concat (rev w), concat (rev c))
  in
  let p, w, c = loop 0 [] [] [] in
  let b = Buffer.create (8 * String.length s) in
  List.iter (fun x -> Buffer.add_int32_be b (Int32.of_int x)) p;
  Buffer.add_int32_be b 0l;
  List.iter (Buffer.add_uint16_be b) w;
  Buffer.add_uint16_be b 0;
  List.iter (Buffer.add_uint8 b) c;
  Buffer.add_uint8 b 0;
  Buffer.add_string b s;
  Buffer.contents b

(* the sort keys already computed, for the tailoring [!current] *)
let keys = Hashtbl.create 1024

let compare a b =
  let key_of s =
    match Hashtbl.find_opt keys s with
    | Some k -> k
    | None ->
        if Hashtbl.length keys >= 100_000 then Hashtbl.reset keys;
        let k = key !current s in
        Hashtbl.add keys s k;
        k
  in
  if a = b then 0 else String.compare (key_of a) (key_of b)

let tailorings = Hashtbl.create 4

let set_lang lang =
  let lang =
    match String.index_opt lang '-' with
    | Some i -> String.sub lang 0 i
    | None -> lang
  in
  let t =
    match List.assoc_opt lang rules with
    | None -> root
    | Some r -> (
        match Hashtbl.find_opt tailorings lang with
        | Some t -> t
        | None ->
            let t = tailor r in
            Hashtbl.replace tailorings lang t;
            t)
  in
  if t != !current then (
    current := t;
    Hashtbl.reset keys)
