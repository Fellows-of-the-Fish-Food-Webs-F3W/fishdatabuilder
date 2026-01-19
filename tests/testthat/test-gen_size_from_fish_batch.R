options(mc.cores = 1)

batch_test <- structure(list(lop_id = c(2287257L, 2206696L, 3877352L, 3130579L,
    4295910L, 2933315L, 4459126L, 4260512L, 2106269L, 3156183L, 3415133L,
    3423985L, 3476067L, 2019100L, 3528567L, 403246L, 225080L, 3831626L,
    1008166L, 4169035L), lop_pre_id = c(34751L, 32440L, 60582L, 47397L,
        71886L, 45424L, 76342L, 69226L, 29965L, 47991L, 53870L, 52829L,
        54047L, 30210L, 54012L, 9665L, 7078L, 62902L, 18288L, 69892L),
    type_batch = c("I", "I", "I", "I", "I", "S/L", "S/L", "S/L",
        "S/L", "S/L", "G", "G", "G", "G", "G", "N", "N", "N", "N",
        "N"), species = c("CHA", "PSR", "LOF", "GOU", "PES", "GOU",
        "TRF", "CHA", "GOU", "CHE", "CHA", "GOU", "GOU", "CHE", "GOU",
        "VAI", "GAR", "TRF", "LOF", "VAN"), lop_longueur_specimens_taille_mini = c(NA,
        NA, NA, NA, NA, 55L, NA, 33L, 76L, 117L, 80L, 60L, 110L,
        NA, 40L, NA, NA, NA, NA, NA), lop_longueur_specimens_taille_maxi = c(NA,
        NA, NA, NA, NA, 90L, NA, 82L, 115L, 190L, 89L, 69L, 119L,
        NA, 49L, NA, NA, NA, NA, NA), lop_numero_batch_wama = c(5L,
        18L, 15L, 2L, 2L, 1L, NA, 7L, 5L, 8L, 0L, 0L, 0L, 0L, 0L,
        0L, 0L, 0L, 0L, 0L), lop_poids = c(85L, 12L, 6L, 71L, 52L,
        475L, 153L, 589L, 818L, 1942L, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA), lop_poids_estime = c(NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, 8L, 16L, 16L, NA, 7L, NA, NA, NA, NA, NA),
    lop_mep_id = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 4L,
        4L, 4L, NA, 4L, NA, NA, NA, NA, NA), lop_effectif = c(7L,
        8L, 3L, 20L, 3L, 123L, 38L, 216L, 120L, 46L, 1L, 5L, 1L,
        88L, 6L, 1L, 1L, 1L, 1L, 1L), lop_type_longueur = c("T",
        "T", "T", "F", "T", "T", NA, "T", "T", "T", "T", "T", "T",
        "T", "T", "F", "T", "T", "T", "T")), row.names = c(NA, -20L
    ), .Names = c("lop_id", "lop_pre_id", "type_batch", "species",
        "lop_longueur_specimens_taille_mini", "lop_longueur_specimens_taille_maxi",
        "lop_numero_batch_wama", "lop_poids", "lop_poids_estime", "lop_mep_id",
        "lop_effectif", "lop_type_longueur"), class = c("tbl_df", "tbl",
        "data.frame"))
measure_test <- structure(list(mei_id = c(231804L, 509234L, 1336205L, 2564256L,
    2564257L, 2564258L, 2564259L, 2564260L, 2564261L, 2564262L, 2564263L,
    2564264L, 2564265L, 2564266L, 2564267L, 2564268L, 2564269L, 2564270L,
    2564271L, 2564272L, 2564273L, 2564274L, 2564275L, 2564276L, 2564277L,
    2564278L, 2564279L, 2564280L, 2564281L, 2564282L, 2564283L, 2564284L,
    2564285L, 3026391L, 3026395L, 3026393L, 3026390L, 3026398L, 3026389L,
    3026388L, 3026392L, 3148854L, 3148853L, 3148852L, 3148865L, 3148862L,
    3148855L, 3148851L, 3779920L, 3779938L, 3779939L, 3779930L, 3779933L,
    3779919L, 3779921L, 3779911L, 3779925L, 3779928L, 3779913L, 3779918L,
    3779937L, 3779917L, 3779940L, 3779922L, 3779916L, 3779923L, 3779912L,
    3779929L, 3779935L, 3779924L, 3779915L, 3779932L, 3779936L, 3779927L,
    3779914L, 3779926L, 3779934L, 3779931L, 4385571L, 4385573L, 4385587L,
    4385586L, 4385572L, 4385580L, 4385589L, 4385581L, 4385575L, 4385584L,
    4385579L, 4385578L, 4385576L, 4385585L, 4385583L, 4385582L, 4385577L,
    4385588L, 4385570L, 4385574L, 4596683L, 4596688L, 4596689L, 4596698L,
    4596684L, 4596694L, 4596676L, 4596692L, 4596695L, 4596697L, 4596677L,
    4596675L, 4596685L, 4596696L, 4596682L, 4596691L, 4596699L, 4596673L,
    4596679L, 4596690L, 4596680L, 4596693L, 4596687L, 4596686L, 4596702L,
    4596681L, 4596678L, 4596674L, 4596700L, 4596701L, 5591042L, 5541329L,
    5541328L, 5541327L, 6365563L, 6219552L, 6219544L, 6219540L, 6219549L,
    6219555L, 6219546L, 6219542L, 6219545L, 6219548L, 6219551L, 6219541L,
    6219547L, 6219543L, 6219554L, 6219553L, 6219550L, 6414492L, 6414487L,
    6414488L, 6805354L, 6805355L, 6805356L, 6805357L, 6805358L, 6805360L,
    6805361L, 6805363L, 6805364L, 6805365L, 6805367L, 6805368L, 6805369L,
    6805371L, 6805372L, 6805378L, 6805380L, 6805382L, 6805383L, 6805385L,
    6805386L, 6805389L, 6805390L, 6805392L, 6805394L, 6805396L, 6805397L,
    6805399L, 6805401L, 6805403L), mei_lop_id = c(225080L, 403246L,
        1008166L, 2106269L, 2106269L, 2106269L, 2106269L, 2106269L, 2106269L,
        2106269L, 2106269L, 2106269L, 2106269L, 2106269L, 2106269L, 2106269L,
        2106269L, 2106269L, 2106269L, 2106269L, 2106269L, 2106269L, 2106269L,
        2106269L, 2106269L, 2106269L, 2106269L, 2106269L, 2106269L, 2106269L,
        2106269L, 2106269L, 2106269L, 2206696L, 2206696L, 2206696L, 2206696L,
        2206696L, 2206696L, 2206696L, 2206696L, 2287257L, 2287257L, 2287257L,
        2287257L, 2287257L, 2287257L, 2287257L, 2933315L, 2933315L, 2933315L,
        2933315L, 2933315L, 2933315L, 2933315L, 2933315L, 2933315L, 2933315L,
        2933315L, 2933315L, 2933315L, 2933315L, 2933315L, 2933315L, 2933315L,
        2933315L, 2933315L, 2933315L, 2933315L, 2933315L, 2933315L, 2933315L,
        2933315L, 2933315L, 2933315L, 2933315L, 2933315L, 2933315L, 3130579L,
        3130579L, 3130579L, 3130579L, 3130579L, 3130579L, 3130579L, 3130579L,
        3130579L, 3130579L, 3130579L, 3130579L, 3130579L, 3130579L, 3130579L,
        3130579L, 3130579L, 3130579L, 3130579L, 3130579L, 3156183L, 3156183L,
        3156183L, 3156183L, 3156183L, 3156183L, 3156183L, 3156183L, 3156183L,
        3156183L, 3156183L, 3156183L, 3156183L, 3156183L, 3156183L, 3156183L,
        3156183L, 3156183L, 3156183L, 3156183L, 3156183L, 3156183L, 3156183L,
        3156183L, 3156183L, 3156183L, 3156183L, 3156183L, 3156183L, 3156183L,
        3831626L, 3877352L, 3877352L, 3877352L, 4169035L, 4260512L, 4260512L,
        4260512L, 4260512L, 4260512L, 4260512L, 4260512L, 4260512L, 4260512L,
        4260512L, 4260512L, 4260512L, 4260512L, 4260512L, 4260512L, 4260512L,
        4295910L, 4295910L, 4295910L, 4459126L, 4459126L, 4459126L, 4459126L,
        4459126L, 4459126L, 4459126L, 4459126L, 4459126L, 4459126L, 4459126L,
        4459126L, 4459126L, 4459126L, 4459126L, 4459126L, 4459126L, 4459126L,
        4459126L, 4459126L, 4459126L, 4459126L, 4459126L, 4459126L, 4459126L,
        4459126L, 4459126L, 4459126L, 4459126L, 4459126L), mei_sex_id = c(NA,
        1L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA), mei_taille = c(138L, 57L, 82L, 115L, 105L,
        90L, 95L, 110L, 109L, 87L, 94L, 98L, 114L, 106L, 81L, 105L, 111L,
        104L, 101L, 92L, 81L, 84L, 110L, 100L, 79L, 91L, 79L, 76L, 82L,
        76L, 106L, 83L, 76L, 45L, 45L, 50L, 40L, 45L, 40L, 45L, 45L,
        96L, 100L, 104L, 95L, 103L, 91L, 92L, 76L, 67L, 75L, 63L, 60L,
        55L, 66L, 76L, 60L, 75L, 76L, 86L, 60L, 65L, 71L, 59L, 70L, 72L,
        74L, 80L, 66L, 90L, 75L, 77L, 69L, 82L, 55L, 67L, 70L, 64L, 83L,
        85L, 75L, 73L, 75L, 74L, 76L, 67L, 80L, 75L, 81L, 76L, 82L, 72L,
        72L, 74L, 78L, 82L, 72L, 62L, 163L, 173L, 145L, 186L, 176L, 144L,
        180L, 160L, 190L, 190L, 157L, 176L, 166L, 185L, 170L, 155L, 180L,
        140L, 170L, 164L, 117L, 148L, 168L, 167L, 145L, 153L, 170L, 176L,
        150L, 175L, 164L, 52L, 57L, 56L, 261L, 53L, 76L, 73L, 52L, 82L,
        39L, 33L, 37L, 69L, 66L, 60L, 77L, 56L, 42L, 62L, 35L, 72L, 59L,
        82L, 83L, 71L, 79L, 76L, 69L, 68L, 71L, 87L, 79L, 70L, 65L, 88L,
        91L, 74L, 72L, 72L, 69L, 78L, 62L, 86L, 66L, 73L, 78L, 65L, 65L,
        75L, 62L, 69L, 77L, 67L), mei_poids = c(28L, NA, 0L, 0L, 0L,
        0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
        0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0L, 0L, 0L, 0L, 0L,
        0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
        0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0L, 0L, 0L,
        0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
        0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 56L, NA, NA, NA,
        168L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
        0L, 0L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA), mei_poids_estime = c(NA, 2L, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    mei_mep_id = c(NA, 4L, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA), mei_age = c(NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
        NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_
    ), mei_mesure_reelle = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), mei_type_longueur = c("T",
        "F", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T",
        "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T",
        "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T",
        "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T",
        "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T",
        "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T",
        "T", "T", "T", "T", "T", "F", "F", "F", "F", "F", "F", "F",
        "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F",
        "F", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T",
        "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T",
        "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T",
        "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T", "T",
        "T", "T", "T", "T", "T", "T", "T", NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA)), .Names = c("mei_id",
        "lop_id", "mei_sex_id", "mei_taille", "mei_poids", "mei_poids_estime",
        "mei_mep_id", "mei_age", "mei_mesure_reelle", "mei_type_longueur"
    ), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA,
        -182L))

test_that("generate fish works on a sample dataset", {
    expect_message(
        output <- get_size_from_batch(
            batch = batch_test,
            id_var = lop_id,
            batch_type_var = type_batch,
            nb_var = lop_effectif,
            min_var = lop_longueur_specimens_taille_mini,
            max_var = lop_longueur_specimens_taille_mini,
            species = species,
            measure = measure_test,
            size_var = mei_taille)
    )
    expect_s3_class(output, "data.frame")
})

test_that("works with NA", {
    batch <- tibble::tibble(
        id = seq(1:4),
        species = rep(c("Pikachu", "Salameche"), each = 2),
        type = c("I", "N", "S/L", "G"),
        min = c(rep(NA, 3), 1),
        max = c(rep(NA, 3), 2),
        nb = c(5, 1, 50, 10)
    )
    measure <- tibble::tibble(
        id = c(rep(1, 5), 2, rep(3, 30)),
        size = c(seq(10, 15), rnorm(30, 10, 2))
    )

    output <- get_size_from_batch(
        batch = batch,
        id_var = id,
        batch_type_var = type,
        nb_var = nb,
        min_var = min,
        max_var = max,
        species = species,
        measure = measure,
        size_var = size)
    expect_equal(sum(is.na(output[["fish"]])), 0)

    # With Nas:
    measure_na <- measure
    set.seed(123)
    sampled_rows <- sample(seq_along(measure_na$size), 30)
    measure_na$size[sampled_rows] <- NA

    expect_warning(
        output <- get_size_from_batch(
            batch = batch,
            id_var = id,
            batch_type_var = type,
            nb_var = nb,
            min_var = min,
            max_var = max,
            species = species,
            measure = measure_na,
            size_var = size)

    )

})

test_that("test for NA or non standard type", {

    batch <- tibble::tibble(
        lop_id = seq(1:4),
        species = rep("Pikachu", 4),
        type = c(rep("G", 4)),
        min = c(1, 1, 0, 1),
        max = c(3, 2, 2, 2),
        nb = c(20, 15, 50, 10)
    )
    batch$type[1] <- NA

    expect_s3_class(
        output <- get_size_from_batch(
            batch = batch,
            id_var = lop_id,
            batch_type_var = type,
            nb_var = nb,
            min_var = min,
            max_var = max,
            species = species,
            measure = measure_test,
            size_var = mei_taille)
        , "data.frame"
    )

    one <- batch[2,]
    expect_type(
        output <- gen_fish_from_batch(
            id = one[["lop_id"]],
            type = one[["type"]],
            min_size = one[["min"]],
            max_size = one[["max"]],
            nb = one[["nb"]],
            ind_measure = NULL,
            ind_id = NULL,
            ind_size = NULL)
        , "double"
    )

    batch$type[1] <- "grrr"

    expect_s3_class(
        output <- get_size_from_batch(
            batch = batch,
            id_var = lop_id,
            batch_type_var = type,
            nb_var = nb,
            min_var = min,
            max_var = max,
            species = species,
            measure = measure_test,
            size_var = mei_taille)
        ,
        "data.frame"
    )
})

test_that("test for NA or non standard nb", {

    batch <- tibble::tibble(
        lop_id = seq(1:4),
        species = rep("Pikachu", 4),
        type = c(rep("G", 4)),
        min = c(1, 1, 0, 1),
        max = c(3, 2, 2, 2),
        nb = c(10, 15, 50, 10)
    )
    batch$nb[1] <- NA

    expect_message(
        output <- get_size_from_batch(
            batch = batch,
            id_var = lop_id,
            batch_type_var = type,
            nb_var = nb,
            min_var = min,
            max_var = max,
            species = species,
            measure = measure_test,
            size_var = mei_taille),
        "Incorrect effectif has been filtered"
    )

    batch$nb[1] <- 0

    expect_message(
        output <- get_size_from_batch(
            batch = batch,
            id_var = lop_id,
            batch_type_var = type,
            nb_var = nb,
            min_var = min,
            max_var = max,
            species = species,
            measure = measure_test,
            size_var = mei_taille),
        "Incorrect effectif has been filtered"

    )
})

test_that("test for  type G", {

    batch <- tibble::tibble(
        lop_id = seq(1:4),
        species = rep("Pikachu", 4),
        type = c(rep("G", 4)),
        min = c(NA,1, 2, 1),
        max = c(3, 2, 2, 2),
        nb = c(50, 10, 50, 10)
    )

    expect_message(
        output <- get_size_from_batch(
            batch = batch,
            id_var = lop_id,
            batch_type_var = type,
            nb_var = nb,
            min_var = min,
            max_var = max,
            species = species,
            measure = measure_test,
            size_var = mei_taille),
        "incorrect batch G have been filtered"

    )
})

test_that("type S/L works with NA", {

    batch <- tibble::tibble(
        id = seq(1:4),
        species = rep("Pikachu", 4),
        type = rep("S/L", 4),
        min = rep(NA, 4),
        max = rep(NA, 4),
        nb = c(5, 1, 50, 10)
    )
    measure <- tibble::tibble(
        id = c(rep(c(1, 2, 3), each = 30), rep(4, 20)),
        size = c(rnorm(30*3, 50, 4), rnorm(20, 10, 2))
    )
    measure$size[31] <- NA

    output <- get_size_from_batch(
        batch = batch,
        id_var = id,
        batch_type_var = type,
        nb_var = nb,
        min_var = min,
        max_var = max,
        species = species,
        measure = measure,
        size_var = size)

    expect_length(na.omit(unlist(output$size_mm)), 66)
    })

test_that("filter of batch measure measure works", {

    batch <- tibble::tibble(
        id = seq(1:4),
        species = rep("Pikachu", 4),
        type = rep("S/L", 4),
        min = rep(NA, 4),
        max = rep(NA, 4),
        nb = c(5, 1, 5, 1)
    )
    measure <- tibble::tibble(
        id = c(rep(c(1, 2, 3), each = 30), rep(c(4, 5), each = 30)),
        size = c(rnorm(30*3, 50, 4), rnorm(30*2, 10, 2))
    )

    expect_message(
        output <- get_size_from_batch(
            batch = batch,
            id_var = id,
            batch_type_var = type,
            nb_var = nb,
            min_var = min,
            max_var = max,
            species = species,
            measure = measure,
            size_var = size),
        "surnumerous batch in measure were removed")

})
