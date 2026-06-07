# cdCAT 0.1.0

* Initial release.
* Session-based CD-CAT engine via the `CdcatSession` R6 class.
* Support for DINA, DINO, and GDINA cognitive diagnosis models.
* Item selection criteria: KL, PWKL, MPWKL, SHE, sequential, and random.
* Attribute estimation methods: MLE, MAP, and EAP.
* Informative priors over skill profiles via `cdcat_prior()`.
* Content balancing (Kingsbury & Zara, 1991).
* Exposure control (Sympson-Hetter and Randomesque).
* Shadow CAT via user-supplied constraint functions.
* Stopping rules: fixed length, attribute-level posterior threshold, and
  dual posterior threshold.
* Per-step history tracking with `history_df()`.
* Built-in simulated datasets (`cdcat_sim`).
