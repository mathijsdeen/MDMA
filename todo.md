# To do

* Create a plot method for `probeInteraction`.
  * Implement an argument (defaults to `TRUE`):
    * To indicate whether the initially used moderator values should be overridden.
    * If `TRUE`, use Johnson-Neyman, and a large `n.interval.moderator` for smooth plotting.
  * Moderator on x-axis, p-value OR effect on y-axis.
    * Use an argument for this.
  * Vertical line corresponding to alpha level
