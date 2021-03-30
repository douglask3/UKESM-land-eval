PhaseDiff <- function(rs) {
    r = rs[[2]] - rs[[1]]
    r[r<(-6)] = r[r<(-6)] + 12
    r[r>6] = r[r>6] -12
    r = r + 6
    r[r>6] = r[r>6] -12
    r
}
