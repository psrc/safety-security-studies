# Latent Class Analysis Summary


## Variables
- Baseline analysis set: expanded 39-item all-respondent block from factor_analysis/outputs/curated_core_selection.csv (n=2226).
- Retained from the earlier 23-item LCA framing: alert_traveler, bike_comfort_cars, drive_safely, harrassment, overall_safety, safe_clothes, safety_choices, safety_choices_costs, safety_choices_modes, signal_confidence, speed_limits, system_safe, traffic_deaths, traffic_enforcement, traffic_rules, traffic_worry, travel_anxiety, travel_stressful, travel_worry, urgent_worry, worry, worry_destination, worry_security.
- Newly added in the expanded block: active_transport_practical, cost_consideration, dest_proximity, enjoy_travel, environment_emphasis, own_car, physical_close, risk_taking, safe_option, safety_ratings, satisfied, telework_importance, transit_travel, travel_satisfaction, walk_recreation_relax, walk_trips.
- Held out from the baseline all-respondent LCA: avoid_walk_1, avoid_walk_2, disagree_test, drive_worry, fear_assault, fear_assault_2, feel_safe, feel_safe_crash, no_harm_travel, no_safe_travel, shortest_path_assault, shortest_path_fear, traffic_crash_worry, traffic_harrassment.

## Directionality
- Direction coding is inherited directly from factor_analysis/outputs/curated_core_selection.csv so higher values always align with greater burden, lower safety, lower confidence, lower satisfaction, or weaker safety orientation.
- Reverse-coded variables in the baseline set (12): bike_comfort_cars, drive_safely, harrassment, overall_safety, signal_confidence, system_safe, traffic_rules, enjoy_travel, safe_option, satisfied, travel_satisfaction, walk_recreation_relax.

## Class Count Sweep
- Expanded best-BIC class count: 7. Recommended final class count: 7.
- Legacy best-BIC class count: 5.
- 2 classes: BIC=233715.2, entropy=0.875, smallest class=48.3%, non-monotonic items=0/39, non-monotonic factors=0/9, notes=no_immediate_within_run_warning
- 3 classes: BIC=229426.2, entropy=0.905, smallest class=27.6%, non-monotonic items=4/39, non-monotonic factors=1/9, notes=no_immediate_within_run_warning
- 4 classes: BIC=228300.3, entropy=0.898, smallest class=15.2%, non-monotonic items=10/39, non-monotonic factors=3/9, notes=multiple_local_optima_seen_across_random_starts
- 5 classes: BIC=227783, entropy=0.903, smallest class=12.8%, non-monotonic items=23/39, non-monotonic factors=3/9, notes=multiple_local_optima_seen_across_random_starts
- 6 classes: BIC=227653.1, entropy=0.9, smallest class=9.6%, non-monotonic items=31/39, non-monotonic factors=7/9, notes=multiple_local_optima_seen_across_random_starts; single_best_random_start
- 7 classes: BIC=227575, entropy=0.905, smallest class=7.7%, non-monotonic items=37/39, non-monotonic factors=7/9, notes=multiple_local_optima_seen_across_random_starts
- Legacy 23-item comparison:
- 2 classes: BIC=135402.7, entropy=0.871, smallest class=49.9%, non-monotonic items=0/23, non-monotonic factors=0/9, notes=no_immediate_within_run_warning
- 3 classes: BIC=132038.7, entropy=0.889, smallest class=25.5%, non-monotonic items=2/23, non-monotonic factors=0/9, notes=no_immediate_within_run_warning
- 4 classes: BIC=130963.7, entropy=0.871, smallest class=18.6%, non-monotonic items=3/23, non-monotonic factors=2/9, notes=no_immediate_within_run_warning
- 5 classes: BIC=130716.5, entropy=0.873, smallest class=8.9%, non-monotonic items=5/23, non-monotonic factors=3/9, notes=multiple_local_optima_seen_across_random_starts
- 6 classes: BIC=130760.2, entropy=0.856, smallest class=8.0%, non-monotonic items=16/23, non-monotonic factors=6/9, notes=multiple_local_optima_seen_across_random_starts
- 7 classes: BIC=130905.6, entropy=0.855, smallest class=7.7%, non-monotonic items=17/23, non-monotonic factors=7/9, notes=multiple_local_optima_seen_across_random_starts

## Interpretation
- The larger 39-item block produces materially more conceptually distinct classes than the legacy 23-item framing, but the final solution still sits on top of a lower-to-higher safety-burden gradient rather than replacing it entirely. The strongest evidence is that the expanded 7-class solution shows 7 of 9 factors and 37 of 39 items breaking a simple monotonic burden ordering, compared with only 3 of 9 factors and 5 of 23 items in the legacy best-BIC solution.
- The expanded solution remains severity-ordered overall, but several mid-spectrum classes split by domain rather than by simple burden level alone.
- burden_rank_3 (share 13.9%) emphasizes Active And Multimodal Travel Orientation | General Worry And Social Discomfort while de-emphasizing Environmental And Travel Flexibility Attitudes | Travel Dissatisfaction And Low Safety Confidence; elevated items: walk_trips | transit_travel | dest_proximity.
- burden_rank_4 (share 14.2%) emphasizes Enforcement And Speed-Safety Norms | Safety-Driven Travel Tradeoffs while de-emphasizing Active And Multimodal Travel Orientation | General Worry And Social Discomfort; elevated items: active_transport_practical | own_car | environment_emphasis.
- burden_rank_5 (share 14.8%) emphasizes Environmental And Travel Flexibility Attitudes | Safety Anxiety And Security Worry while de-emphasizing System And Rule-Based Safety Confidence | Enforcement And Speed-Safety Norms; elevated items: urgent_worry | worry_security | worry_destination.
- burden_rank_6 (share 14.8%) emphasizes Safety Anxiety And Security Worry | Safety-Driven Travel Tradeoffs while de-emphasizing Risk-Tolerant Travel Confidence | Active And Multimodal Travel Orientation; elevated items: worry_destination | travel_anxiety | worry_security.
- burden_rank_7 (share 7.7%) emphasizes Safety Anxiety And Security Worry | Travel Dissatisfaction And Low Safety Confidence while de-emphasizing Risk-Tolerant Travel Confidence | Active And Multimodal Travel Orientation; elevated items: overall_safety | worry_destination | drive_safely.
- Recommended final solution: 7 classes. It wins on BIC, keeps the smallest class above 7%, and exposes much more cross-domain structure than the legacy framing. The tradeoff is that poLCA still found multiple local optima across random starts, so the segmentation should be treated as moderately stable rather than definitive.
- Remaining limitations: the workflow is unweighted, complete-case, and restricted to the all-respondent block; gated items such as drive_worry and the structural not-applicable frequency battery remain out of scope for this baseline run.

## Output Guide
- See lca_candidate_model_summary.csv for fit, entropy, class shares, and convergence notes.
- See lca_candidate_cross_domain_diagnostics.csv, lca_candidate_factor_monotonicity.csv, and lca_recommended_class_factor_summary.csv for the evidence on monotonicity versus cross-domain patterning.
- See lca_recommended_class_membership.csv, lca_recommended_class_probabilities.csv, and lca_recommended_class_profile_summary.csv for the recommended solution outputs.
