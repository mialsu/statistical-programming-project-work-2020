{
    test_that("data files imported correctly", {
        expect_equal(nrow(tree_measurements), 3550L)
        expect_equal(names(tree_measurements), c("bar_code", "Family", "Genus", "species", "X", "Y", "value", "variable_id"))
        expect_equal(nrow(tree_variables), 11L)
        expect_equal(names(tree_variables), c("variable_id", "variable_name"))
    })
    test_that("tables joined correctly", {
        expect_equal(nrow(tree_tidy), 355L)
        expect_setequal(names(tree_tidy), c("bar_code", "Family", "Genus", "species", "X", "Y", "DBH", "leaf_thickness", "leaf_toughness", "N", "C_N", "N15", "C13", "chlorophyll_concentration", "surface_area", "SLA"))
    })
    test_that("model1 correct", {
        expect_mapequal(model1$coefficients, c(`(Intercept)` = 15.364445463918, DBH = -0.193534688566557))
    })
    test_that("model2 correct", {
        expect_mapequal(model2$coefficients, c(`(Intercept)` = 9.40347631728922, DBH = -0.095020509021409, speciesguyanensis = 2.0510378328503, speciesvenulosa = 7.44056236616972))
    })
    test_that("model3 correct", {
        expect_mapequal(model3$coefficients, c(`(Intercept)` = 8.11342214456947, DBH = -0.0573870705365626, speciesguyanensis = 2.23284351324675, speciesvenulosa = 14.0694827122583, `DBH:speciesguyanensis` = 0.0053222654810711, `DBH:speciesvenulosa` = -0.286977019662218))
    })
    test_that("model_predictions correct", {
        expect_true(all(c("model", "pred", "DBH", "species") %in% names(model_predictions)))
        test <- model_predictions %>% select(DBH, species) %>% group_by(DBH, species) %>% summarize() %>% ungroup() %>% gather_predictions(model1, model2, model3, .pred = "pred_test")
        res <- model_predictions %>% left_join(test)
        expect_equal(unname(res$pred), unname(res$pred_test))
    })
    test_that("model_predictions has sufficiently fine grid", {
        q <- quantile(tree_tidy[["DBH"]], c(0, 0.05, 0.95, 1))
        expect_gte(length(unique(cut(model_predictions[["DBH"]], breaks = q))), 3)
    })
    test_that("model_residuals correct", {
        expect_true(all(c("model", "resid", "DBH", "species", "SLA") %in% names(model_residuals)))
        expect_equal(nrow(model_residuals), 1065L)
        expect_equal(model_residuals %>% group_by(model) %>% summarize(sum_resid2 = sum(resid^2)), structure(list(model = c("model1", "model2", "model3"), sum_resid2 = c(3997.73553418835, 1078.00204448489, 659.132603840356)), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame")))
    })
}