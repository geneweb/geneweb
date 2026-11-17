(* Weights of the default table of the Unicode collation algorithm (DUCET,
   allkeys.txt 17.0.0), primary then secondary, of the lowercase letters
   without canonical decomposition or weighed apart from it (й), and of the
   combining marks of U+0000..U+052F and U+1E00..U+1EFF; the primary weights
   are multiplied by 16, so that a tailoring can insert letters between two of
   them *)
let weights =
  let t = Hashtbl.create 512 in
  List.iter
    (fun (c, p, s) -> Hashtbl.replace t c (List.map (fun w -> w lsl 4) p, s))
    [
      (0x0061, [ 0x23EC ], [ 0x20 ]);
      (0x0062, [ 0x2406 ], [ 0x20 ]);
      (0x0063, [ 0x2420 ], [ 0x20 ]);
      (0x0064, [ 0x2436 ], [ 0x20 ]);
      (0x0065, [ 0x2453 ], [ 0x20 ]);
      (0x0066, [ 0x248E ], [ 0x20 ]);
      (0x0067, [ 0x249D ], [ 0x20 ]);
      (0x0068, [ 0x24C4 ], [ 0x20 ]);
      (0x0069, [ 0x24DF ], [ 0x20 ]);
      (0x006A, [ 0x24FB ], [ 0x20 ]);
      (0x006B, [ 0x2514 ], [ 0x20 ]);
      (0x006C, [ 0x2528 ], [ 0x20 ]);
      (0x006D, [ 0x2562 ], [ 0x20 ]);
      (0x006E, [ 0x2571 ], [ 0x20 ]);
      (0x006F, [ 0x2598 ], [ 0x20 ]);
      (0x0070, [ 0x25C8 ], [ 0x20 ]);
      (0x0071, [ 0x25DD ], [ 0x20 ]);
      (0x0072, [ 0x25F0 ], [ 0x20 ]);
      (0x0073, [ 0x2632 ], [ 0x20 ]);
      (0x0074, [ 0x265D ], [ 0x20 ]);
      (0x0075, [ 0x2680 ], [ 0x20 ]);
      (0x0076, [ 0x26B0 ], [ 0x20 ]);
      (0x0077, [ 0x26C2 ], [ 0x20 ]);
      (0x0078, [ 0x26CC ], [ 0x20 ]);
      (0x0079, [ 0x26D8 ], [ 0x20 ]);
      (0x007A, [ 0x26EE ], [ 0x20 ]);
      (0x00AA, [ 0x23EC ], [ 0x20 ]);
      (0x00B5, [ 0x279F ], [ 0x20 ]);
      (0x00BA, [ 0x2598 ], [ 0x20 ]);
      (0x00DF, [ 0x2632; 0x2632 ], [ 0x20; 0x11F; 0x20 ]);
      (0x00E6, [ 0x23EC; 0x2453 ], [ 0x20; 0x11F; 0x20 ]);
      (0x00F0, [ 0x2436 ], [ 0x20; 0x11F ]);
      (0x00F8, [ 0x2598 ], [ 0x20; 0x2F ]);
      (0x00FE, [ 0x271E ], [ 0x20 ]);
      (0x0111, [ 0x2436 ], [ 0x20; 0x39 ]);
      (0x0127, [ 0x24C4 ], [ 0x20; 0x39 ]);
      (0x0131, [ 0x24E3 ], [ 0x20 ]);
      (0x0133, [ 0x24DF; 0x24FB ], [ 0x20; 0x20 ]);
      (0x0138, [ 0x25EC ], [ 0x20 ]);
      (0x0140, [ 0x2528 ], [ 0x20; 0x11F ]);
      (0x0142, [ 0x2528 ], [ 0x20; 0x39 ]);
      (0x0149, [ 0x274E; 0x2571 ], [ 0x20; 0x20 ]);
      (0x014B, [ 0x2591 ], [ 0x20 ]);
      (0x0153, [ 0x2598; 0x2453 ], [ 0x20; 0x11F; 0x20 ]);
      (0x0167, [ 0x2662 ], [ 0x20 ]);
      (0x017F, [ 0x2632 ], [ 0x20; 0x120 ]);
      (0x0180, [ 0x240E ], [ 0x20 ]);
      (0x0183, [ 0x241B ], [ 0x20 ]);
      (0x0185, [ 0x2741 ], [ 0x20 ]);
      (0x0188, [ 0x242C ], [ 0x20 ]);
      (0x018C, [ 0x2449 ], [ 0x20 ]);
      (0x018D, [ 0x26EE; 0x26C2 ], [ 0x20; 0x20 ]);
      (0x0192, [ 0x2497 ], [ 0x20 ]);
      (0x0195, [ 0x24CC ], [ 0x20 ]);
      (0x0199, [ 0x251A ], [ 0x20 ]);
      (0x019A, [ 0x2533 ], [ 0x20 ]);
      (0x019B, [ 0x2559 ], [ 0x20 ]);
      (0x019E, [ 0x2580 ], [ 0x20 ]);
      (0x01A3, [ 0x24C0 ], [ 0x20 ]);
      (0x01A5, [ 0x25D1 ], [ 0x20 ]);
      (0x01A8, [ 0x2739 ], [ 0x20 ]);
      (0x01AA, [ 0x264F ], [ 0x20 ]);
      (0x01AB, [ 0x2668 ], [ 0x20 ]);
      (0x01AD, [ 0x266C ], [ 0x20 ]);
      (0x01B4, [ 0x26E4 ], [ 0x20 ]);
      (0x01B6, [ 0x26F3 ], [ 0x20 ]);
      (0x01B9, [ 0x2710 ], [ 0x20 ]);
      (0x01BA, [ 0x2716 ], [ 0x20 ]);
      (0x01BB, [ 0x2732 ], [ 0x20 ]);
      (0x01BD, [ 0x273D ], [ 0x20 ]);
      (0x01BE, [ 0x265D; 0x2632 ], [ 0x20; 0x20 ]);
      (0x01BF, [ 0x2725 ], [ 0x20 ]);
      (0x01C0, [ 0x276B ], [ 0x20 ]);
      (0x01C1, [ 0x276F ], [ 0x20 ]);
      (0x01C2, [ 0x2773 ], [ 0x20 ]);
      (0x01C3, [ 0x2777 ], [ 0x20 ]);
      (0x01C6, [ 0x2436; 0x26EE ], [ 0x20; 0x20; 0x28 ]);
      (0x01C9, [ 0x2528; 0x24FB ], [ 0x20; 0x20 ]);
      (0x01CC, [ 0x2571; 0x24FB ], [ 0x20; 0x20 ]);
      (0x01DD, [ 0x2461 ], [ 0x20 ]);
      (0x01E5, [ 0x24AC ], [ 0x20 ]);
      (0x01F3, [ 0x2436; 0x26EE ], [ 0x20; 0x20 ]);
      (0x021D, [ 0x26EA ], [ 0x20 ]);
      (0x0221, [ 0x244D ], [ 0x20 ]);
      (0x0223, [ 0x25C3 ], [ 0x20 ]);
      (0x0225, [ 0x26F9 ], [ 0x20 ]);
      (0x0234, [ 0x254D ], [ 0x20 ]);
      (0x0235, [ 0x258B ], [ 0x20 ]);
      (0x0236, [ 0x2676 ], [ 0x20 ]);
      (0x0237, [ 0x24FF ], [ 0x20 ]);
      (0x0238, [ 0x2436; 0x2406 ], [ 0x20; 0x20 ]);
      (0x0239, [ 0x25DD; 0x25C8 ], [ 0x20; 0x20 ]);
      (0x023C, [ 0x2425 ], [ 0x20 ]);
      (0x023F, [ 0x2641 ], [ 0x20 ]);
      (0x0240, [ 0x2705 ], [ 0x20 ]);
      (0x0242, [ 0x2749 ], [ 0x20 ]);
      (0x0247, [ 0x245A ], [ 0x20 ]);
      (0x0249, [ 0x2504 ], [ 0x20 ]);
      (0x024B, [ 0x25E8 ], [ 0x20 ]);
      (0x024D, [ 0x25FB ], [ 0x20 ]);
      (0x024F, [ 0x26E0 ], [ 0x20 ]);
      (0x0250, [ 0x23F7 ], [ 0x20 ]);
      (0x0251, [ 0x23FB ], [ 0x20 ]);
      (0x0252, [ 0x2401 ], [ 0x20 ]);
      (0x0253, [ 0x2417 ], [ 0x20 ]);
      (0x0254, [ 0x25AC ], [ 0x20 ]);
      (0x0255, [ 0x2430 ], [ 0x20 ]);
      (0x0256, [ 0x243F ], [ 0x20 ]);
      (0x0257, [ 0x2443 ], [ 0x20 ]);
      (0x0258, [ 0x2470 ], [ 0x20 ]);
      (0x0259, [ 0x2466 ], [ 0x20 ]);
      (0x025A, [ 0x2474 ], [ 0x20 ]);
      (0x025B, [ 0x246B ], [ 0x20 ]);
      (0x025C, [ 0x2478 ], [ 0x20 ]);
      (0x025D, [ 0x247E ], [ 0x20 ]);
      (0x025E, [ 0x2482 ], [ 0x20 ]);
      (0x025F, [ 0x250C ], [ 0x20 ]);
      (0x0260, [ 0x24B1 ], [ 0x20 ]);
      (0x0261, [ 0x24A2 ], [ 0x20 ]);
      (0x0262, [ 0x24A8 ], [ 0x20 ]);
      (0x0263, [ 0x24BC ], [ 0x20 ]);
      (0x0264, [ 0x248A ], [ 0x20 ]);
      (0x0265, [ 0x2695 ], [ 0x20 ]);
      (0x0266, [ 0x24D1 ], [ 0x20 ]);
      (0x0267, [ 0x24D9 ], [ 0x20 ]);
      (0x0268, [ 0x24EE ], [ 0x20 ]);
      (0x0269, [ 0x24F6 ], [ 0x20 ]);
      (0x026A, [ 0x24E7 ], [ 0x20 ]);
      (0x026B, [ 0x2538 ], [ 0x20 ]);
      (0x026C, [ 0x253E ], [ 0x20 ]);
      (0x026D, [ 0x2546 ], [ 0x20 ]);
      (0x026E, [ 0x2552 ], [ 0x20 ]);
      (0x026F, [ 0x26A1 ], [ 0x20 ]);
      (0x0270, [ 0x26A7 ], [ 0x20 ]);
      (0x0271, [ 0x2569 ], [ 0x20 ]);
      (0x0272, [ 0x257C ], [ 0x20 ]);
      (0x0273, [ 0x2586 ], [ 0x20 ]);
      (0x0274, [ 0x2575 ], [ 0x20 ]);
      (0x0275, [ 0x25B9 ], [ 0x20 ]);
      (0x0276, [ 0x259F ], [ 0x20 ]);
      (0x0277, [ 0x25BE ], [ 0x20 ]);
      (0x0278, [ 0x25D8 ], [ 0x20 ]);
      (0x0279, [ 0x2600 ], [ 0x20 ]);
      (0x027A, [ 0x2605 ], [ 0x20 ]);
      (0x027B, [ 0x260A ], [ 0x20 ]);
      (0x027C, [ 0x2610 ], [ 0x20 ]);
      (0x027D, [ 0x2614 ], [ 0x20 ]);
      (0x027E, [ 0x261A ], [ 0x20 ]);
      (0x027F, [ 0x2620 ], [ 0x20 ]);
      (0x0280, [ 0x25F5 ], [ 0x20 ]);
      (0x0281, [ 0x2629 ], [ 0x20 ]);
      (0x0282, [ 0x263B ], [ 0x20 ]);
      (0x0283, [ 0x2648 ], [ 0x20 ]);
      (0x0284, [ 0x2510 ], [ 0x20 ]);
      (0x0285, [ 0x2653 ], [ 0x20 ]);
      (0x0286, [ 0x2657 ], [ 0x20 ]);
      (0x0287, [ 0x267B ], [ 0x20 ]);
      (0x0288, [ 0x2670 ], [ 0x20 ]);
      (0x0289, [ 0x268B ], [ 0x20 ]);
      (0x028A, [ 0x26AB ], [ 0x20 ]);
      (0x028B, [ 0x26B7 ], [ 0x20 ]);
      (0x028C, [ 0x26BE ], [ 0x20 ]);
      (0x028D, [ 0x26C8 ], [ 0x20 ]);
      (0x028E, [ 0x255D ], [ 0x20 ]);
      (0x028F, [ 0x26DC ], [ 0x20 ]);
      (0x0290, [ 0x26FD ], [ 0x20 ]);
      (0x0291, [ 0x2701 ], [ 0x20 ]);
      (0x0292, [ 0x270B ], [ 0x20 ]);
      (0x0293, [ 0x271A ], [ 0x20 ]);
      (0x0294, [ 0x2745 ], [ 0x20 ]);
      (0x0295, [ 0x2754 ], [ 0x20 ]);
      (0x0296, [ 0x2766 ], [ 0x20 ]);
      (0x0297, [ 0x277C ], [ 0x20 ]);
      (0x0298, [ 0x2781 ], [ 0x20 ]);
      (0x0299, [ 0x240A ], [ 0x20 ]);
      (0x029A, [ 0x2486 ], [ 0x20 ]);
      (0x029B, [ 0x24B5 ], [ 0x20 ]);
      (0x029C, [ 0x24C8 ], [ 0x20 ]);
      (0x029D, [ 0x2508 ], [ 0x20 ]);
      (0x029E, [ 0x2523 ], [ 0x20 ]);
      (0x029F, [ 0x252C ], [ 0x20 ]);
      (0x02A0, [ 0x25E4 ], [ 0x20 ]);
      (0x02A1, [ 0x275E ], [ 0x20 ]);
      (0x02A2, [ 0x2762 ], [ 0x20 ]);
      (0x02A3, [ 0x2436; 0x26EE ], [ 0x20; 0x20 ]);
      (0x02A4, [ 0x2436; 0x270B ], [ 0x20; 0x20 ]);
      (0x02A5, [ 0x2436; 0x2701 ], [ 0x20; 0x20 ]);
      (0x02A6, [ 0x265D; 0x2632 ], [ 0x20; 0x20 ]);
      (0x02A7, [ 0x265D; 0x2648 ], [ 0x20; 0x20 ]);
      (0x02A8, [ 0x265D; 0x2430 ], [ 0x20; 0x20 ]);
      (0x02A9, [ 0x248E; 0x2591 ], [ 0x20; 0x20 ]);
      (0x02AA, [ 0x2528; 0x2632 ], [ 0x20; 0x20 ]);
      (0x02AB, [ 0x2528; 0x26EE ], [ 0x20; 0x20 ]);
      (0x02AC, [ 0x2785 ], [ 0x20 ]);
      (0x02AD, [ 0x2789 ], [ 0x20 ]);
      (0x02AE, [ 0x2699 ], [ 0x20 ]);
      (0x02AF, [ 0x269D ], [ 0x20 ]);
      (0x0300, [], [ 0x25 ]);
      (0x0301, [], [ 0x24 ]);
      (0x0302, [], [ 0x27 ]);
      (0x0303, [], [ 0x2D ]);
      (0x0304, [], [ 0x32 ]);
      (0x0305, [], [ 0x3A ]);
      (0x0306, [], [ 0x26 ]);
      (0x0307, [], [ 0x2E ]);
      (0x0308, [], [ 0x2B ]);
      (0x0309, [], [ 0x3B ]);
      (0x030A, [], [ 0x29 ]);
      (0x030B, [], [ 0x2C ]);
      (0x030C, [], [ 0x28 ]);
      (0x030D, [], [ 0x33 ]);
      (0x030E, [], [ 0x33 ]);
      (0x030F, [], [ 0x3C ]);
      (0x0310, [], [ 0x3D ]);
      (0x0311, [], [ 0x3E ]);
      (0x0312, [], [ 0x33 ]);
      (0x0313, [], [ 0x22 ]);
      (0x0314, [], [ 0x23 ]);
      (0x0315, [], [ 0x33 ]);
      (0x0316, [], [ 0x34 ]);
      (0x0317, [], [ 0x34 ]);
      (0x0318, [], [ 0x34 ]);
      (0x0319, [], [ 0x34 ]);
      (0x031A, [], [ 0x33 ]);
      (0x031B, [], [ 0x3F ]);
      (0x031C, [], [ 0x34 ]);
      (0x031D, [], [ 0x34 ]);
      (0x031E, [], [ 0x34 ]);
      (0x031F, [], [ 0x34 ]);
      (0x0320, [], [ 0x34 ]);
      (0x0321, [], [ 0x40 ]);
      (0x0322, [], [ 0x41 ]);
      (0x0323, [], [ 0x42 ]);
      (0x0324, [], [ 0x43 ]);
      (0x0325, [], [ 0x44 ]);
      (0x0326, [], [ 0x45 ]);
      (0x0327, [], [ 0x30 ]);
      (0x0328, [], [ 0x31 ]);
      (0x0329, [], [ 0x34 ]);
      (0x032A, [], [ 0x34 ]);
      (0x032B, [], [ 0x34 ]);
      (0x032C, [], [ 0x34 ]);
      (0x032D, [], [ 0x46 ]);
      (0x032E, [], [ 0x47 ]);
      (0x032F, [], [ 0x34 ]);
      (0x0330, [], [ 0x48 ]);
      (0x0331, [], [ 0x49 ]);
      (0x0332, [], [ 0x21 ]);
      (0x0333, [], [ 0x34 ]);
      (0x0334, [], [ 0x4A ]);
      (0x0335, [], [ 0x39 ]);
      (0x0336, [], [ 0x35 ]);
      (0x0337, [], [ 0x35 ]);
      (0x0338, [], [ 0x2F ]);
      (0x0339, [], [ 0x4B ]);
      (0x033A, [], [ 0x34 ]);
      (0x033B, [], [ 0x34 ]);
      (0x033C, [], [ 0x34 ]);
      (0x033D, [], [ 0x33 ]);
      (0x033E, [], [ 0x33 ]);
      (0x033F, [], [ 0x33 ]);
      (0x0342, [], [ 0x2A ]);
      (0x0345, [], [ 0x4C ]);
      (0x0346, [], [ 0x33 ]);
      (0x0347, [], [ 0x34 ]);
      (0x0348, [], [ 0x34 ]);
      (0x0349, [], [ 0x34 ]);
      (0x034A, [], [ 0x33 ]);
      (0x034B, [], [ 0x33 ]);
      (0x034C, [], [ 0x33 ]);
      (0x034D, [], [ 0x34 ]);
      (0x034E, [], [ 0x34 ]);
      (0x0350, [], [ 0x33 ]);
      (0x0351, [], [ 0x33 ]);
      (0x0352, [], [ 0x33 ]);
      (0x0353, [], [ 0x34 ]);
      (0x0354, [], [ 0x34 ]);
      (0x0355, [], [ 0x34 ]);
      (0x0356, [], [ 0x34 ]);
      (0x0357, [], [ 0x33 ]);
      (0x0358, [], [ 0x4D ]);
      (0x0359, [], [ 0x34 ]);
      (0x035A, [], [ 0x34 ]);
      (0x035B, [], [ 0x33 ]);
      (0x035C, [], [ 0x34 ]);
      (0x035D, [], [ 0x33 ]);
      (0x035E, [], [ 0x33 ]);
      (0x035F, [], [ 0x34 ]);
      (0x0360, [], [ 0x4E ]);
      (0x0361, [], [ 0x4F ]);
      (0x0362, [], [ 0x34 ]);
      (0x0363, [ 0x23EC ], [ 0x20 ]);
      (0x0364, [ 0x2453 ], [ 0x20 ]);
      (0x0365, [ 0x24DF ], [ 0x20 ]);
      (0x0366, [ 0x2598 ], [ 0x20 ]);
      (0x0367, [ 0x2680 ], [ 0x20 ]);
      (0x0368, [ 0x2420 ], [ 0x20 ]);
      (0x0369, [ 0x2436 ], [ 0x20 ]);
      (0x036A, [ 0x24C4 ], [ 0x20 ]);
      (0x036B, [ 0x2562 ], [ 0x20 ]);
      (0x036C, [ 0x25F0 ], [ 0x20 ]);
      (0x036D, [ 0x265D ], [ 0x20 ]);
      (0x036E, [ 0x26B0 ], [ 0x20 ]);
      (0x036F, [ 0x26CC ], [ 0x20 ]);
      (0x0371, [ 0x2797 ], [ 0x20 ]);
      (0x0373, [ 0x27B8 ], [ 0x20 ]);
      (0x0377, [ 0x2794 ], [ 0x20 ]);
      (0x037A, [ 0x279A ], [ 0x20 ]);
      (0x037B, [ 0x27AD ], [ 0x20 ]);
      (0x037C, [ 0x27AC ], [ 0x20 ]);
      (0x037D, [ 0x27AE ], [ 0x20 ]);
      (0x03B1, [ 0x278D ], [ 0x20 ]);
      (0x03B2, [ 0x278E ], [ 0x20 ]);
      (0x03B3, [ 0x278F ], [ 0x20 ]);
      (0x03B4, [ 0x2791 ], [ 0x20 ]);
      (0x03B5, [ 0x2792 ], [ 0x20 ]);
      (0x03B6, [ 0x2796 ], [ 0x20 ]);
      (0x03B7, [ 0x2798 ], [ 0x20 ]);
      (0x03B8, [ 0x2799 ], [ 0x20 ]);
      (0x03B9, [ 0x279A ], [ 0x20 ]);
      (0x03BA, [ 0x279C ], [ 0x20 ]);
      (0x03BB, [ 0x279D ], [ 0x20 ]);
      (0x03BC, [ 0x279F ], [ 0x20 ]);
      (0x03BD, [ 0x27A0 ], [ 0x20 ]);
      (0x03BE, [ 0x27A1 ], [ 0x20 ]);
      (0x03BF, [ 0x27A2 ], [ 0x20 ]);
      (0x03C0, [ 0x27A3 ], [ 0x20 ]);
      (0x03C1, [ 0x27A8 ], [ 0x20 ]);
      (0x03C2, [ 0x27AB ], [ 0x20 ]);
      (0x03C3, [ 0x27AB ], [ 0x20 ]);
      (0x03C4, [ 0x27AF ], [ 0x20 ]);
      (0x03C5, [ 0x27B0 ], [ 0x20 ]);
      (0x03C6, [ 0x27B1 ], [ 0x20 ]);
      (0x03C7, [ 0x27B2 ], [ 0x20 ]);
      (0x03C8, [ 0x27B3 ], [ 0x20 ]);
      (0x03C9, [ 0x27B5 ], [ 0x20 ]);
      (0x03D0, [ 0x278E ], [ 0x20 ]);
      (0x03D1, [ 0x2799 ], [ 0x20 ]);
      (0x03D2, [ 0x27B0 ], [ 0x20 ]);
      (0x03D5, [ 0x27B1 ], [ 0x20 ]);
      (0x03D6, [ 0x27A3 ], [ 0x20 ]);
      (0x03D7, [ 0x279C; 0x278D; 0x279A ], [ 0x20; 0x20; 0x20 ]);
      (0x03D9, [ 0x27A7 ], [ 0x20 ]);
      (0x03DB, [ 0x2795 ], [ 0x20 ]);
      (0x03DD, [ 0x2793 ], [ 0x20 ]);
      (0x03DF, [ 0x27A6 ], [ 0x20 ]);
      (0x03E1, [ 0x27B7 ], [ 0x20 ]);
      (0x03E3, [ 0x27D9 ], [ 0x20 ]);
      (0x03E5, [ 0x27DE ], [ 0x20 ]);
      (0x03E7, [ 0x27DF ], [ 0x20 ]);
      (0x03E9, [ 0x27E2 ], [ 0x20 ]);
      (0x03EB, [ 0x27E9 ], [ 0x20 ]);
      (0x03ED, [ 0x27EC ], [ 0x20 ]);
      (0x03EF, [ 0x27F0 ], [ 0x20 ]);
      (0x03F0, [ 0x279C ], [ 0x20 ]);
      (0x03F1, [ 0x27A8 ], [ 0x20 ]);
      (0x03F2, [ 0x27AB ], [ 0x20 ]);
      (0x03F3, [ 0x279B ], [ 0x20 ]);
      (0x03F5, [ 0x2792 ], [ 0x20 ]);
      (0x03F8, [ 0x27B9 ], [ 0x20 ]);
      (0x03FB, [ 0x27A5 ], [ 0x20 ]);
      (0x03FC, [ 0x27AA ], [ 0x20 ]);
      (0x0430, [ 0x27F6 ], [ 0x20 ]);
      (0x0431, [ 0x2802 ], [ 0x20 ]);
      (0x0432, [ 0x2806 ], [ 0x20 ]);
      (0x0433, [ 0x280A ], [ 0x20 ]);
      (0x0434, [ 0x281E ], [ 0x20 ]);
      (0x0435, [ 0x282E ], [ 0x20 ]);
      (0x0436, [ 0x2836 ], [ 0x20 ]);
      (0x0437, [ 0x2840 ], [ 0x20 ]);
      (0x0438, [ 0x2854 ], [ 0x20 ]);
      (0x0439, [ 0x2861 ], [ 0x20 ]);
      (0x043A, [ 0x286A ], [ 0x20 ]);
      (0x043B, [ 0x2884 ], [ 0x20 ]);
      (0x043C, [ 0x2897 ], [ 0x20 ]);
      (0x043D, [ 0x28A0 ], [ 0x20 ]);
      (0x043E, [ 0x28BB ], [ 0x20 ]);
      (0x043F, [ 0x28C3 ], [ 0x20 ]);
      (0x0440, [ 0x28D0 ], [ 0x20 ]);
      (0x0441, [ 0x28D9 ], [ 0x20 ]);
      (0x0442, [ 0x28E2 ], [ 0x20 ]);
      (0x0443, [ 0x28F2 ], [ 0x20 ]);
      (0x0444, [ 0x2903 ], [ 0x20 ]);
      (0x0445, [ 0x2907 ], [ 0x20 ]);
      (0x0446, [ 0x292E ], [ 0x20 ]);
      (0x0447, [ 0x2939 ], [ 0x20 ]);
      (0x0448, [ 0x2958 ], [ 0x20 ]);
      (0x0449, [ 0x295D ], [ 0x20 ]);
      (0x044A, [ 0x2964 ], [ 0x20 ]);
      (0x044B, [ 0x2969 ], [ 0x20 ]);
      (0x044C, [ 0x296D ], [ 0x20 ]);
      (0x044D, [ 0x297A ], [ 0x20 ]);
      (0x044E, [ 0x297E ], [ 0x20 ]);
      (0x044F, [ 0x2984 ], [ 0x20 ]);
      (0x0452, [ 0x2824 ], [ 0x20 ]);
      (0x0454, [ 0x2832 ], [ 0x20 ]);
      (0x0455, [ 0x2848 ], [ 0x20 ]);
      (0x0456, [ 0x285C ], [ 0x20 ]);
      (0x0458, [ 0x2865 ], [ 0x20 ]);
      (0x0459, [ 0x2890 ], [ 0x20 ]);
      (0x045A, [ 0x28B6 ], [ 0x20 ]);
      (0x045B, [ 0x28EE ], [ 0x20 ]);
      (0x045F, [ 0x2954 ], [ 0x20 ]);
      (0x0461, [ 0x291D ], [ 0x20 ]);
      (0x0463, [ 0x2975 ], [ 0x20 ]);
      (0x0465, [ 0x2989 ], [ 0x20 ]);
      (0x0467, [ 0x298D ], [ 0x20 ]);
      (0x0469, [ 0x2997 ], [ 0x20 ]);
      (0x046B, [ 0x2992 ], [ 0x20 ]);
      (0x046D, [ 0x299C ], [ 0x20 ]);
      (0x046F, [ 0x29A0 ], [ 0x20 ]);
      (0x0471, [ 0x29A4 ], [ 0x20 ]);
      (0x0473, [ 0x29A8 ], [ 0x20 ]);
      (0x0475, [ 0x29AC ], [ 0x20 ]);
      (0x0479, [ 0x28FF ], [ 0x20 ]);
      (0x047B, [ 0x292A ], [ 0x20 ]);
      (0x047D, [ 0x2926 ], [ 0x20 ]);
      (0x047F, [ 0x2921 ], [ 0x20 ]);
      (0x0481, [ 0x28CC ], [ 0x20 ]);
      (0x0483, [], [ 0x50 ]);
      (0x0484, [], [ 0x33 ]);
      (0x0485, [], [ 0x23 ]);
      (0x0486, [], [ 0x22 ]);
      (0x0487, [], [ 0x33 ]);
      (0x048B, [ 0x2858 ], [ 0x20 ]);
      (0x048D, [ 0x2971 ], [ 0x20 ]);
      (0x048F, [ 0x28D4 ], [ 0x20 ]);
      (0x0491, [ 0x280A ], [ 0x20; 0x120 ]);
      (0x0493, [ 0x280E ], [ 0x20 ]);
      (0x0495, [ 0x2816 ], [ 0x20 ]);
      (0x0497, [ 0x283C ], [ 0x20 ]);
      (0x0499, [ 0x282A ], [ 0x20 ]);
      (0x049B, [ 0x286E ], [ 0x20 ]);
      (0x049D, [ 0x287E ], [ 0x20 ]);
      (0x049F, [ 0x287A ], [ 0x20 ]);
      (0x04A1, [ 0x2876 ], [ 0x20 ]);
      (0x04A3, [ 0x28A9 ], [ 0x20 ]);
      (0x04A5, [ 0x28B2 ], [ 0x20 ]);
      (0x04A7, [ 0x28C8 ], [ 0x20 ]);
      (0x04A9, [ 0x29B1 ], [ 0x20 ]);
      (0x04AB, [ 0x28DE ], [ 0x20 ]);
      (0x04AD, [ 0x28E9 ], [ 0x20 ]);
      (0x04AF, [ 0x28F6 ], [ 0x20 ]);
      (0x04B1, [ 0x28FA ], [ 0x20 ]);
      (0x04B3, [ 0x2913 ], [ 0x20 ]);
      (0x04B5, [ 0x2934 ], [ 0x20 ]);
      (0x04B7, [ 0x293F ], [ 0x20 ]);
      (0x04B9, [ 0x2947 ], [ 0x20 ]);
      (0x04BB, [ 0x2917 ], [ 0x20 ]);
      (0x04BD, [ 0x294C ], [ 0x20 ]);
      (0x04BF, [ 0x2950 ], [ 0x20 ]);
      (0x04C4, [ 0x2872 ], [ 0x20 ]);
      (0x04C6, [ 0x2889 ], [ 0x20 ]);
      (0x04C8, [ 0x28AD ], [ 0x20 ]);
      (0x04CA, [ 0x28A5 ], [ 0x20 ]);
      (0x04CC, [ 0x2943 ], [ 0x20 ]);
      (0x04CE, [ 0x289B ], [ 0x20 ]);
      (0x04CF, [ 0x29B6 ], [ 0x20 ]);
      (0x04D5, [ 0x27FE ], [ 0x20 ]);
      (0x04D9, [ 0x27FA ], [ 0x20 ]);
      (0x04E1, [ 0x284D ], [ 0x20 ]);
      (0x04E9, [ 0x28BF ], [ 0x20 ]);
      (0x04F7, [ 0x281A ], [ 0x20 ]);
      (0x04FB, [ 0x2812 ], [ 0x20 ]);
      (0x04FD, [ 0x290B ], [ 0x20 ]);
      (0x04FF, [ 0x290F ], [ 0x20 ]);
      (0x0501, [ 0x2822 ], [ 0x20 ]);
      (0x0503, [ 0x2829 ], [ 0x20 ]);
      (0x0505, [ 0x2845 ], [ 0x20 ]);
      (0x0507, [ 0x2852 ], [ 0x20 ]);
      (0x0509, [ 0x2895 ], [ 0x20 ]);
      (0x050B, [ 0x28BA ], [ 0x20 ]);
      (0x050D, [ 0x28DD ], [ 0x20 ]);
      (0x050F, [ 0x28E8 ], [ 0x20 ]);
      (0x0511, [ 0x2846 ], [ 0x20 ]);
      (0x0513, [ 0x288E ], [ 0x20 ]);
      (0x0515, [ 0x2896 ], [ 0x20 ]);
      (0x0517, [ 0x28D8 ], [ 0x20 ]);
      (0x0519, [ 0x2988 ], [ 0x20 ]);
      (0x051B, [ 0x2883 ], [ 0x20 ]);
      (0x051D, [ 0x29B5 ], [ 0x20 ]);
      (0x051F, [ 0x2882 ], [ 0x20 ]);
      (0x0521, [ 0x288F ], [ 0x20 ]);
      (0x0523, [ 0x28B1 ], [ 0x20 ]);
      (0x0525, [ 0x28C7 ], [ 0x20 ]);
      (0x0527, [ 0x291B ], [ 0x20 ]);
      (0x0529, [ 0x28A4 ], [ 0x20 ]);
      (0x052B, [ 0x283A ], [ 0x20 ]);
      (0x052D, [ 0x293D ], [ 0x20 ]);
      (0x052F, [ 0x288D ], [ 0x20 ]);
      (0x1E9A, [ 0x23EC; 0x2750 ], [ 0x20; 0x20 ]);
      (0x1E9C, [ 0x2646 ], [ 0x20 ]);
      (0x1E9D, [ 0x2647 ], [ 0x20 ]);
      (0x1E9F, [ 0x2452 ], [ 0x20 ]);
      (0x1EFB, [ 0x2528; 0x2528 ], [ 0x20; 0x20 ]);
      (0x1EFD, [ 0x26BD ], [ 0x20 ]);
      (0x1EFF, [ 0x26E8 ], [ 0x20 ]);
    ];
  t

let ascii =
  Array.init 128 (fun c ->
      match
        Hashtbl.find_opt weights (Char.code (Char.lowercase_ascii (Char.chr c)))
      with
      | Some (p, _) -> p
      | None -> [ c lsl 4 ])

(* Standard tailorings of the Common Locale Data Repository (CLDR 50,
   common/collation/<language>.xml) for the languages of GeneWeb whose order
   differs from the common one *)
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
    ( "da",
      {|[caseFirst upper]
       &D<<đ<<<Đ<<ð<<<Ð
       &th<<<þ
       &TH<<<Þ
       &Y<<ü<<<Ü<<ű<<<Ű
       &[before 1]ǀ<æ<<<Æ<<ä<<<Ä<ø<<<Ø<<ö<<<Ö<<ő<<<Ő<å<<<Å<<<aa<<<Aa<<<AA
       &oe<<œ<<<Œ|}
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

(* the primary and secondary weights of the character at [i] in [s], and the
   position of the next one; the letters of other scripts after these ones,
   by code point, the other characters by their transliteration *)
let root_char t s i =
  let c = Char.code (String.unsafe_get s i) in
  if c < 0x80 then (ascii.(c), [ 0x20 ], i + 1)
  else
    let j = next s i in
    let ch = String.sub s i (j - i) in
    let u = Uchar.utf_decode_uchar (String.get_utf_8_uchar s i) in
    let us =
      if Hashtbl.mem weights (Uchar.to_int (lower u)) then [ u ]
      else decompose ch
    in
    let p, w =
      List.split
      @@ List.map
           (fun u ->
             let l = Uchar.to_int (lower u) in
             match Hashtbl.find_opt weights l with
             | Some (p, w) when t.cyrillic_first && l >= 0x400 && l < 0x530 ->
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
    (List.concat p, List.concat w, j)

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

(* The sort key of [s] (Unicode collation algorithm, 7.3): its primary,
   secondary and tertiary weights, the tertiary being the case of each
   character, then [s] itself, so that only equal strings have equal keys *)
let key t s =
  let rec loop i p w c =
    if i < String.length s then
      let p', w', j = char t s i in
      let upper =
        match s.[i] with
        | 'A' .. 'Z' -> true
        | '\000' .. '\127' -> false
        | _ ->
            let u = Uchar.utf_decode_uchar (String.get_utf_8_uchar s i) in
            Uucp.Case.is_upper u || Uucp.Gc.general_category u = `Lt
      in
      let case = if not upper then 2 else if t.upper_first then 1 else 8 in
      loop j (p' :: p) (w' :: w) (case :: c)
    else (List.concat (List.rev p), List.concat (List.rev w), List.rev c)
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
